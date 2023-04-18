/*
    see copyright notice in squirrel.h
*/
#include "sqpcheader.h"
#ifndef NO_COMPILER
#include <stdarg.h>
#include <ctype.h>
#include <setjmp.h>
#include <algorithm>
#include "sqopcodes.h"
#include "sqstring.h"
#include "sqfuncproto.h"
#include "sqcompiler.h"
#include "sqfuncstate.h"
#include "sqoptimizer.h"
#include "sqlexer.h"
#include "sqvm.h"
#include "sqtable.h"
#include "sqast.h"

#include <iostream>
#include <ostream>

#define EXPR   1
#define OBJECT 2
#define BASE   3
#define LOCAL  4
#define OUTER  5

struct SQExpState {
  SQInteger  etype;       /* expr. type; one of EXPR, OBJECT, BASE, OUTER or LOCAL */
  SQInteger  epos;        /* expr. location on stack; -1 for OBJECT and BASE */
  bool       donot_get;   /* signal not to deref the next value */
  bool       is_assignable_var; // for LOCAL and OUTER
  bool       literal_field;

  bool isBinding() { return (etype==LOCAL || etype==OUTER) && !is_assignable_var; }
};

#ifndef SQ_LINE_INFO_IN_STRUCTURES
#  define SQ_LINE_INFO_IN_STRUCTURES 1
#endif

#define MAX_COMPILER_ERROR_LEN 256
#define MAX_FUNCTION_NAME_LEN 128

struct SQScope {
    SQInteger outers;
    SQInteger stacksize;
};

enum SQExpressionContext
{
  SQE_REGULAR = 0,
  SQE_IF,
  SQE_SWITCH,
  SQE_LOOP_CONDITION,
  SQE_FUNCTION_ARG,
  SQE_RVALUE,
};



class RenderVisitor : public Visitor {


    const char* treeopToStr(enum TreeOp op) {
        switch (op)
        {
        case TO_NULLC: return "??";
        case TO_ASSIGN: return "=";
        case TO_OROR: return "||";
        case TO_ANDAND: return "&&";
        case TO_OR: return "|";
        case TO_XOR: return "^";
        case TO_AND: return "&";
        case TO_NE: return "!=";
        case TO_EQ: return "==";
        case TO_3CMP: return "<=>";
        case TO_GE: return ">=";
        case TO_GT: return ">";
        case TO_LE: return "<=";
        case TO_LT: return "<";
        case TO_IN: return "IN";
        case TO_INSTANCEOF: "INSTANCEOF";
        case TO_USHR: return ">>>";
        case TO_SHR: return ">>";
        case TO_SHL: return "<<";
        case TO_MUL: return "*";
        case TO_DIV: return "/";
        case TO_MOD: return "%";
        case TO_ADD: return "+";
        case TO_SUB: return "-";
        case TO_NOT: return "!";
        case TO_BNOT: return "~";
        case TO_NEG: return "-";
        case TO_TYPEOF: return "TYPEOF ";
        case TO_RESUME: return "RESUME ";
        case TO_CLONE: return "CLONE ";
        case TO_DELETE: return "DELETE ";
        case TO_PINC: return "++";
        case TO_PDEC: return "--";
        case TO_INEXPR_ASSIGN: return ":=";
        case TO_NEWSLOT: return " <- ";
        case TO_PLUSEQ: return " += ";
        case TO_MINUSEQ: return " -= ";
        case TO_MULEQ: return " *= ";
        case TO_DIVEQ: return " /= ";
        case TO_MODEQ: return " %= ";
        default: return "<UNKNOWN>";
        }
    }

    void indent(int ind) {
        for (int i = 0; i < ind; ++i) _out << ' ';
    }

public:
    RenderVisitor(std::ostream &output) : _out(output), _indent(0) {}

    std::ostream &_out;
    SQInteger _indent;

    virtual void visitUnExpr(UnExpr *expr) {
        if (expr->op() == TO_PAREN) {
            _out << "(";
        }
        else {
            _out << treeopToStr(expr->op());
        }
        expr->_arg->visit(*this);
        if (expr->op() == TO_PAREN) {
            _out << ")";
        }
    }
    virtual void visitBinExpr(BinExpr *expr) {
        expr->_lhs->visit(*this);
        _out << treeopToStr(expr->op());
        expr->_rhs->visit(*this);
    }
    virtual void visitTerExpr(TerExpr *expr) { 
        expr->a()->visit(*this);
        _out << " ? ";
        expr->b()->visit(*this);
        _out << " : ";
        expr->c()->visit(*this);
    }
    virtual void visitCallExpr(CallExpr *expr) { 
        expr->_callee->visit(*this);
        _out << "(";
        for (int i = 0; i < expr->_args.size(); ++i) {
            if (i) _out << ", ";
            expr->_args[i]->visit(*this);
        }
        _out << ")";

    }
    virtual void visitId(Id *id) { _out << _stringval(id->_id); }

    virtual void visitGetFieldExpr(GetFieldExpr *expr) { 
        expr->receiver()->visit(*this);
        if (expr->isNullable()) _out << '?';
        _out << '.';
        _out << expr->fieldName();
    }
    virtual void visitSetFieldExpr(SetFieldExpr *expr) {
        expr->receiver()->visit(*this);
        if (expr->isNullable()) _out << '?';
        _out << '.';
        _out << expr->fieldName();
        _out << " = ";
        expr->value()->visit(*this);
    }
    virtual void visitGetTableExpr(GetTableExpr *expr) {
        expr->receiver()->visit(*this);
        if (expr->isNullable()) _out << '?';
        _out << '[';
        expr->key()->visit(*this);
        _out << ']';
    }
    virtual void visitSetTableExpr(SetTableExpr *expr) {
        expr->receiver()->visit(*this);
        if (expr->isNullable()) _out << '?';
        _out << '[';
        expr->key()->visit(*this);
        _out << ']';
        _out << " = ";
        expr->value()->visit(*this);
    }
    virtual void visitBaseExpr(BaseExpr *expr) { _out << "base"; }
    virtual void visitRootExpr(RootExpr *expr) { _out << "::"; }
    virtual void visitThisExpr(ThisExpr *expr) { _out << "this"; }
    virtual void visitCalleeExpr(CalleeExpr *expr) { _out << "<callee>";; }
    virtual void visitLiteralExpr(LiteralExpr *expr) {
        switch (expr->kind())
        {
        case LK_STRING: _out << '"' << expr->s() << '"'; break;
        case LK_FLOAT: _out << expr->f(); break;
        case LK_INT: _out << expr->i(); break;
        case LK_BOOL: _out << expr->b(); break;
        case LK_NULL: _out << expr->null(); break;
        default: assert(0);
            break;
        };
    }
    virtual void visitIncExpr(IncExpr *expr) { 

        const char *op = expr->_diff < 0 ? "--" : "++";

        if (expr->form() == IF_PREFIX) _out << op;
        expr->_arg->visit(*this);
        if (expr->form() == IF_POSTFIX) _out << op;
    }

    virtual void visitArrayExpr(ArrayExpr *expr) { 
        _out << "[";
        for (int i = 0; i < expr->_inits.size(); ++i) {
            if (i) _out << ", ";
            expr->_inits[i]->visit(*this);
        }
        _out << "]";
    }

    virtual void visitCommaExpr(CommaExpr *expr) { 
        for (int i = 0; i < expr->_exprs.size(); ++i) {
            if (i) _out << ", ";
            expr->_exprs[i]->visit(*this);
        }
    }

    //virtual void visitDeclExpr(DeclExpr *expr) {
    //    expr->_decl->visit(*this);
    //}

    virtual void visitBlock(Block *block) { 
        _out << "{" << std::endl;
        int cur = _indent;
        _indent += 2;
        const sqvector<Statement *> &stmts = block->statements();
        for (int i = 0; i < stmts.size(); ++i) {
            indent(_indent);
            stmts[i]->visit(*this);
            _out << std::endl;
        }
        indent(cur);
        _out << "}";
        _indent = cur;
    }
    virtual void visitIfStatement(IfStatement *ifstmt) { 
        _out << "IF (";
        ifstmt->_cond->visit(*this);
        _out << ")" << std::endl;
        indent(_indent);
        _out << "THEN ";
        _indent += 2;
        ifstmt->_thenB->visit(*this);
        if (ifstmt->_elseB) {
            _out << std::endl;
            indent(_indent - 2);
            _out << "ELSE ";
            ifstmt->_elseB->visit(*this);
        }
        _indent -= 2;
        _out << std::endl;
        indent(_indent);
        _out << "END_IF";

    }
    virtual void visitLoopStatement(LoopStatement *loop) { 
        indent(_indent);
        loop->_body->visit(*this);
    }
    virtual void visitWhileStatement(WhileStatement *loop) {
        _out << "WHILE (";
        loop->_cond->visit(*this);
        _out << ")" << std::endl;
        _indent += 2;
        visitLoopStatement(loop);
        _indent -= 2;
        indent(_indent);
        _out << "END_WHILE";

    }
    virtual void visitDoWhileStatement(DoWhileStatement *loop) { 
        _out << "DO";

        _indent += 2;
        visitLoopStatement(loop);
        _indent -= 2;

        indent(_indent);
        _out << "WHILE (" << std::endl;
        loop->_cond->visit(*this);
        _out << ")";
    }
    virtual void visitForStatement(ForStatement *loop) { 
        _out << "FOR (";

        if (loop->_init) loop->_init->visit(*this);
        _out << "; ";

        if (loop->_cond) loop->_cond->visit(*this);
        _out << "; ";

        if (loop->_mod) loop->_mod->visit(*this);
        _out << ")" << std::endl;


        _indent += 2;
        visitLoopStatement(loop);
        _indent -= 2;

        _out << std::endl;

        indent(_indent);
        _out << "END_FOR" << std::endl;
    }
    virtual void visitForeachStatement(ForeachStatement *loop) { 
        _out << "FOR_EACH ( {";

        visitId(loop->_idx);
        _out << ", ";

        visitId(loop->_val);
        _out << "} in ";

        loop->_container->visit(*this);
        _out << ")" << std::endl;

        _indent += 2;
        visitLoopStatement(loop);
        _indent -= 2;

        _out << std::endl;
        indent(_indent);
        _out << "END_FOREACH";
    }
    virtual void visitSwitchStatement(SwitchStatement *swtch) { 
        _out << "SWITCH (";
        swtch->_expr->visit(*this);
        _out << ")" << std::endl;
        int cur = _indent;
        _indent += 2;
        
        for (int i = 0; i < swtch->cases().size(); ++i) {
            SwitchCase &c = swtch->cases()[i];
            indent(cur);
            _out << "CASE ";
            c.val->visit(*this);
            _out << ": ";
            c.stmt->visit(*this);
            _out << std::endl;
        }

        if (swtch->_defaultCase.stmt) {
            indent(cur);
            _out << "DEFAULT: ";
            swtch->_defaultCase.stmt->visit(*this);
            _out << std::endl;
        }

        _indent -= 2;
    }
    virtual void visitTryStatement(TryStatement *tr) {
        _out << "TRY" << std::endl;
        _indent += 2;
        indent(_indent);
        tr->_tryStmt->visit(*this);
        _out << std::endl;
        _out << "CATCH (";
        visitId(tr->_exception);
        _out << ")";
        indent(_indent);
        tr->_catchStmt->visit(*this);
        _out << std::endl;
        indent(_indent -= 2);
        _out << "END_TRY";
    }
    virtual void visitTerminateStatement(TerminateStatement *term) { if (term->_arg) term->_arg->visit(*this); }
    virtual void visitReturnStatement(ReturnStatement *ret) { 
        _out << "RETURN ";
        visitTerminateStatement(ret);
    }
    virtual void visitYieldStatement(YieldStatement *yld) {
        _out << "YIELD ";
        visitTerminateStatement(yld);
    }
    virtual void visitThrowStatement(ThrowStatement *thr) {
        _out << "THROW ";
        visitTerminateStatement(thr);
    }
    //virtual void visitJumpStatement(JumpStatement *jmp) { visitStmt(jmp); }
    virtual void visitBreakStatement(BreakStatement *jmp) { _out << "BREAK"; }
    virtual void visitContinueStatement(ContinueStatement *jmp) { _out << "CONTINUE"; }
    virtual void visitExprStatement(ExprStatement *estmt) { estmt->_expr->visit(*this); }
    virtual void visitEmptyStatement(EmptyStatement *empty) { _out << ";"; }

    virtual void visitValueDecl(ValueDecl *decl) { 
        visitId(decl->_name);
        if (decl->_expr) {
            _out << " = ";
            decl->_expr->visit(*this);
        }
    }
    //virtual void visitVarDecl(VarDecl *decl) { visitValueDecl(decl); }
    //virtual void visitParamDecl(ParamDecl *decl) { visitValueDecl(decl); }
    virtual void visitTableDecl(TableDecl *tbl) { 
        _out << "{" << std::endl;
        _indent += 2;
        for (int i = 0; i < tbl->_members.size(); ++i) {
            TableMember &m = tbl->_members[i];
            indent(_indent);
            if (m.isStatic) _out << "STATIC ";
            m.key->visit(*this);
            _out << " <- ";
            m.value->visit(*this);
            _out << std::endl;
        }
        _indent -= 2;
        indent(_indent);
        _out << "}";
    }
    virtual void visitClassDecl(ClassDecl *cls) { 
        _out << "CLASS ";
        if (cls->classKey()) cls->classKey()->visit(*this);
        if (cls->classBase()) {
            _out << " EXTENDS ";
            cls->classBase()->visit(*this);
            _out << ' ';
        }
        visitTableDecl(cls);
    }
    virtual void visitFunctionDecl(FunctionDecl *f) {
        _out << "FUNCTION";
        if (f->_name) {
            _out << " ";
            visitId(f->_name);
        }

        _out << '(';
        for (int i = 0; i < f->parameters().size(); ++i) {
            if (i) _out << ", ";
            visitParamDecl(f->parameters()[i]);
        }

        if (f->_vararg) {
            _out << ", ...";
        }
        _out << ") ";

        f->_body->visit(*this);
    }
    virtual void visitConstructorDecl(ConstructorDecl *ctr) { visitFunctionDecl(ctr); }
    virtual void visitConstDecl(ConstDecl *cnst) {
        if (cnst->_global) _out << "G ";
        visitId(cnst->_id);
        _out << " = ";
        _out << "(TODO -- RENDER OBJECT)";
    }
    virtual void visitEnumDecl(EnumDecl *enm) { 
        _out << "ENUM ";
        visitId(enm->_id);
        _out << std::endl;
        _indent += 2;
        for (int i = 0; i < enm->_consts.size(); ++i) {
            EnumConst &c = enm->_consts[i];
            indent(_indent);
            visitId(enm->_id);
            _out << " = ";
            _out << "(TODO -- RENDER OBJECT)" << std::endl;
        }
        _indent -= 2;
        indent(_indent);
        _out << "END_ENUM";
    }

    virtual void visitDeclGroup(DeclGroup *grp) {
        for (int i = 0; i < grp->declarations().size(); ++i) {
            if (i) _out << ", ";
            grp->declarations()[i]->visit(*this);
        }
    }

    virtual void visitDesctructionDecl(DesctructionDecl *destruct) {
        _out << "{ ";
        for (int i = 0; i < destruct->declarations().size(); ++i) {
            if (i) _out << ", ";
            destruct->declarations()[i]->visit(*this);
        }
        _out << " } = ";
        destruct->_expr->visit(*this);
    }
};


#define BEGIN_SCOPE() SQScope __oldscope__ = _scope; \
                     _scope.outers = _fs->_outers; \
                     _scope.stacksize = _fs->GetStackSize(); \
                     _scopedconsts.push_back();

#define RESOLVE_OUTERS() if(_fs->GetStackSize() != _fs->_blockstacksizes.top()) { \
                            if(_fs->CountOuters(_fs->_blockstacksizes.top())) { \
                                _fs->AddInstruction(_OP_CLOSE,0,_fs->_blockstacksizes.top()); \
                            } \
                        }

#define END_SCOPE_NO_CLOSE() {  if(_fs->GetStackSize() != _scope.stacksize) { \
                            _fs->SetStackSize(_scope.stacksize); \
                        } \
                        _scope = __oldscope__; \
                        assert(!_scopedconsts.empty()); \
                        _scopedconsts.pop_back(); \
                    }

#define END_SCOPE() {   SQInteger oldouters = _fs->_outers;\
                        if(_fs->GetStackSize() != _scope.stacksize) { \
                            _fs->SetStackSize(_scope.stacksize); \
                            if(oldouters != _fs->_outers) { \
                                _fs->AddInstruction(_OP_CLOSE,0,_scope.stacksize); \
                            } \
                        } \
                        _scope = __oldscope__; \
                        _scopedconsts.pop_back(); \
                    }

#define BEGIN_BREAKBLE_BLOCK()  SQInteger __nbreaks__=_fs->_unresolvedbreaks.size(); \
                            SQInteger __ncontinues__=_fs->_unresolvedcontinues.size(); \
                            _fs->_breaktargets.push_back(0);_fs->_continuetargets.push_back(0); \
                            _fs->_blockstacksizes.push_back(_scope.stacksize);


#define END_BREAKBLE_BLOCK(continue_target) {__nbreaks__=_fs->_unresolvedbreaks.size()-__nbreaks__; \
                    __ncontinues__=_fs->_unresolvedcontinues.size()-__ncontinues__; \
                    if(__ncontinues__>0)ResolveContinues(_fs,__ncontinues__,continue_target); \
                    if(__nbreaks__>0)ResolveBreaks(_fs,__nbreaks__); \
                    _fs->_breaktargets.pop_back();_fs->_continuetargets.pop_back(); \
                    _fs->_blockstacksizes.pop_back(); }


class SQCompiler;

class CodegenVisitor : public Visitor {

    SQFuncState *_fs;
    SQFuncState *_funcState;
    SQCompiler &_compiler;
    SQVM &_vm;

    SQScope _scope;
    SQObjectPtrVec _scopedconsts;
    SQInteger _stacksize;

    SQObjectPtr &_o;

    bool _donot_get;

    SQInteger _num_initial_bindings;
public:
    CodegenVisitor(SQAllocContext ctx, SQFuncState &root_fs, const HSQOBJECT *bindings, SQCompiler &compiler, SQVM *vm, SQObjectPtr sourceName, SQObjectPtr &o);

    void finish();

    void Emit2ArgsOP(SQOpcode op, SQInteger p3 = 0);

    void EmitLoadConstInt(SQInteger value, SQInteger target);

    void EmitLoadConstFloat(SQFloat value, SQInteger target);

    void ResolveBreaks(SQFuncState *funcstate, SQInteger ntoresolve);
    void ResolveContinues(SQFuncState *funcstate, SQInteger ntoresolve, SQInteger targetpos);

    void EmitDerefOp(SQOpcode op);

    void visitBlock(Block *block) override;

    void visitIfStatement(IfStatement *ifStmt) override;

    void visitWhileStatement(WhileStatement *whileLoop) override;

    void visitDoWhileStatement(DoWhileStatement *doWhileLoop) override;

    void visitForStatement(ForStatement *forLoop) override;

    void visitForeachStatement(ForeachStatement *foreachLoop) override;

    void visitSwitchStatement(SwitchStatement *swtch) override;

    void visitTryStatement(TryStatement *tryStmt) override;

    void visitBreakStatement(BreakStatement *breakStmt) override;

    void visitContinueStatement(ContinueStatement *continueStmt) override;

    void visitTerminateStatement(TerminateStatement *terminator) override;

    void visitReturnStatement(ReturnStatement *retStmt) override;

    void visitYieldStatement(YieldStatement *yieldStmt) override;

    void visitThrowStatement(ThrowStatement *throwStmt) override;

    void visitExprStatement(ExprStatement *stmt) override;

    void visitTableDecl(TableDecl *tableDecl) override;

    void visitClassDecl(ClassDecl *klass) override;

    void visitParamDecl(ParamDecl *param) override;

    SQInteger _last_pop = -1;

    void visitVarDecl(VarDecl *var) override;

    void visitDeclGroup(DeclGroup *group) override;

    void visitDesctructionDecl(DesctructionDecl *destruct) override;

    void visitFunctionDecl(FunctionDecl *func) override;

    SQTable* GetScopedConstsTable();

    void visitConstDecl(ConstDecl *decl) override;

    void visitEnumDecl(EnumDecl *enums) override;

    void MoveIfCurrentTargetIsLocal();

    void visitCallExpr(CallExpr *call) override;

    void visitBaseExpr(BaseExpr *base) override;

    void visitCalleeExpr(CalleeExpr *expr) override;

    void visitRootExpr(RootExpr *expr) override;

    void visitThisExpr(ThisExpr *expr) override;

    void visitLiteralExpr(LiteralExpr *lit) override;

    void visitArrayExpr(ArrayExpr *expr) override;

    void emitUnaryOp(SQOpcode op, Expr *arg);

    void emitDelete(Expr *argument);

    void visitUnExpr(UnExpr *unary) override;

    void emitSimpleBin(SQOpcode op, Expr *lhs, Expr *rhs, SQInteger op3 = 0);

    void emitJpmArith(SQOpcode op, Expr *lhs, Expr *rhs);

    void emitCompoundArith(SQOpcode op, SQInteger opcode, Expr *lvalue, Expr *rvalue);

    void emitNewSlot(Expr *lvalue, Expr *rvalue);

    void emitAssign(Expr *lvalue, Expr * rvalue, bool inExpr);

    void emitFieldAssign(bool isLiteral);

    bool CanBeDefaultDelegate(const SQChar *key);

    bool canBeLiteral(AccessExpr *expr);

    void visitGetFieldExpr(GetFieldExpr *expr) override;

    void visitGetTableExpr(GetTableExpr *expr) override;

    void visitBinExpr(BinExpr *expr) override;

    void visitTerExpr(TerExpr *expr) override;

    void visitIncExpr(IncExpr *expr) override;

    bool IsConstant(const SQObject &name, SQObject &e);

    bool IsLocalConstant(const SQObject &name, SQObject &e);

    bool IsGlobalConstant(const SQObject &name, SQObject &e);

    void visitId(Id *id) override;


};

class SQCompiler
{
public:
    SQCompiler(SQVM *v, SQLEXREADFUNC rg, SQUserPointer up, const HSQOBJECT *bindings, const SQChar* sourcename, bool raiseerror, bool lineinfo) :
      _lex(_ss(v)),
      _scopedconsts(_ss(v)->_alloc_ctx),
      _member_constant_keys_check(_ss(v)->_alloc_ctx)
    {
        _vm=v;
        _lex.Init(_ss(v), rg, up,ThrowError,this);
        _sourcename = SQString::Create(_ss(v), sourcename);
        _lineinfo = lineinfo;_raiseerror = raiseerror;
        _scope.outers = 0;
        _scope.stacksize = 0;
        _compilererror[0] = _SC('\0');
        _expression_context = SQE_REGULAR;
        _num_initial_bindings = 0;

        if (bindings) {
            assert(sq_type(*bindings)==OT_TABLE || sq_type(*bindings)==OT_NULL);
            if (sq_type(*bindings)==OT_TABLE) {
                _scopedconsts.push_back(*bindings);
                _num_initial_bindings = 1;
            }
        }
    }

    SQObjectPtrVec scopedconsts() { return _scopedconsts; }

    bool IsConstant(const SQObject &name,SQObject &e)
    {
        if (IsLocalConstant(name, e))
            return true;
        if (IsGlobalConstant(name, e))
            return true;
        return false;
    }

    bool IsLocalConstant(const SQObject &name,SQObject &e)
    {
        SQObjectPtr val;
        for (SQInteger i=SQInteger(_scopedconsts.size())-1; i>=0; --i) {
            SQObjectPtr &tbl = _scopedconsts[i];
            if (!sq_isnull(tbl) && _table(tbl)->Get(name,val)) {
                e = val;
                if (tbl._flags & SQOBJ_FLAG_IMMUTABLE)
                    e._flags |= SQOBJ_FLAG_IMMUTABLE;
                return true;
            }
        }
        return false;
    }

    bool IsGlobalConstant(const SQObject &name,SQObject &e)
    {
        SQObjectPtr val;
        if(_table(_ss(_vm)->_consts)->Get(name,val)) {
            e = val;
            return true;
        }
        return false;
    }

    static void ThrowError(void *ud, const SQChar *s) {
        SQCompiler *c = (SQCompiler *)ud;
        c->Error(s);
    }
    void Error(const SQChar *s, ...)
    {
        va_list vl;
        va_start(vl, s);
        scvsprintf(_compilererror, MAX_COMPILER_ERROR_LEN, s, vl);
        va_end(vl);
        longjmp(_errorjmp,1);
    }

    SQUnsignedInteger _lang_features;
    void ProcessDirective()
    {
        const SQChar *sval = _lex._svalue;

        if (scstrncmp(sval, _SC("pos:"), 4) == 0) {
            sval += 4;
            if (!scisdigit(*sval))
                Error(_SC("expected line number after #pos:"));
            SQChar * next = NULL;
            _lex._currentline = scstrtol(sval, &next, 10);
            if (!next || *next != ':')
                Error(_SC("expected ':'"));
            next++;
            if (!scisdigit(*next))
                Error(_SC("expected column number after #pos:<line>:"));
            _lex._currentcolumn = scstrtol(next, NULL, 10);

            return;
        }

        SQInteger setFlags = 0, clearFlags = 0;
        bool applyToDefault = false;
        if (scstrncmp(sval, _SC("default:"), 8) == 0) {
            applyToDefault = true;
            sval += 8;
        }

        if (scstrcmp(sval, _SC("strict")) == 0)
            setFlags = LF_STRICT;
        else if (scstrcmp(sval, _SC("relaxed")) == 0)
            clearFlags = LF_STRICT;
        else if (scstrcmp(sval, _SC("strict-bool")) == 0)
            setFlags = LF_STRICT_BOOL;
        else if (scstrcmp(sval, _SC("relaxed-bool")) == 0)
            clearFlags = LF_STRICT_BOOL;
        else if (scstrcmp(sval, _SC("no-root-fallback")) == 0)
            setFlags = LF_EXPLICIT_ROOT_LOOKUP;
        else if (scstrcmp(sval, _SC("implicit-root-fallback")) == 0)
            clearFlags = LF_EXPLICIT_ROOT_LOOKUP;
        else if (scstrcmp(sval, _SC("no-func-decl-sugar")) == 0)
            setFlags = LF_NO_FUNC_DECL_SUGAR;
        else if (scstrcmp(sval, _SC("allow-func-decl-sugar")) == 0)
            clearFlags = LF_NO_FUNC_DECL_SUGAR;
        else if (scstrcmp(sval, _SC("no-class-decl-sugar")) == 0)
            setFlags = LF_NO_CLASS_DECL_SUGAR;
        else if (scstrcmp(sval, _SC("allow-class-decl-sugar")) == 0)
            clearFlags = LF_NO_CLASS_DECL_SUGAR;
        else if (scstrcmp(sval, _SC("no-plus-concat")) == 0)
            setFlags = LF_NO_PLUS_CONCAT;
        else if (scstrcmp(sval, _SC("allow-plus-concat")) == 0)
            clearFlags = LF_NO_PLUS_CONCAT;
        else if (scstrcmp(sval, _SC("explicit-this")) == 0)
            setFlags = LF_EXPLICIT_THIS;
        else if (scstrcmp(sval, _SC("implicit-this")) == 0)
            clearFlags = LF_EXPLICIT_THIS;
        else if (scstrcmp(sval, _SC("forbid-root-table")) == 0)
            setFlags = LF_FORBID_ROOT_TABLE;
        else if (scstrcmp(sval, _SC("allow-root-table")) == 0)
            clearFlags = LF_FORBID_ROOT_TABLE;
        else if (scstrcmp(sval, _SC("disable-optimizer")) == 0)
            setFlags = LF_DISABLE_OPTIMIZER;
        else if (scstrcmp(sval, _SC("enable-optimizer")) == 0)
            clearFlags = LF_DISABLE_OPTIMIZER;
        else
            Error(_SC("unsupported directive"));

        _lang_features = _fs->lang_features = (_fs->lang_features | setFlags) & ~clearFlags;
        if (applyToDefault)
            _ss(_vm)->defaultLangFeatures = (_ss(_vm)->defaultLangFeatures | setFlags) & ~clearFlags;
    }

    void Lex()
    {
        _token = _lex.Lex();
        while (_token == TK_DIRECTIVE)
        {
            bool endOfLine = (_lex._prevtoken == _SC('\n'));
            ProcessDirective();
            _token = _lex.Lex();
            if (endOfLine)
                _lex._prevtoken = _SC('\n');
        }
    }


    SQObject Expect(SQInteger tok)
    {

        if(_token != tok) {
            if(_token == TK_CONSTRUCTOR && tok == TK_IDENTIFIER) {
                //do nothing
            }
            else {
                const SQChar *etypename;
                if(tok > 255) {
                    switch(tok)
                    {
                    case TK_IDENTIFIER:
                        etypename = _SC("IDENTIFIER");
                        break;
                    case TK_STRING_LITERAL:
                        etypename = _SC("STRING_LITERAL");
                        break;
                    case TK_INTEGER:
                        etypename = _SC("INTEGER");
                        break;
                    case TK_FLOAT:
                        etypename = _SC("FLOAT");
                        break;
                    default:
                        etypename = _lex.Tok2Str(tok);
                    }
                    Error(_SC("expected '%s'"), etypename);
                }
                Error(_SC("expected '%c'"), tok);
            }
        }
        SQObjectPtr ret;
        switch(tok)
        {
        case TK_IDENTIFIER:
            ret = _fs->CreateString(_lex._svalue);
            break;
        case TK_STRING_LITERAL:
            ret = _fs->CreateString(_lex._svalue,_lex._longstr.size()-1);
            break;
        case TK_INTEGER:
            ret = SQObjectPtr(_lex._nvalue);
            break;
        case TK_FLOAT:
            ret = SQObjectPtr(_lex._fvalue);
            break;
        }
        Lex();
        return ret;
    }
    bool IsEndOfStatement() { return ((_lex._prevtoken == _SC('\n')) || (_token == SQUIRREL_EOB) || (_token == _SC('}')) || (_token == _SC(';'))); }
    void OptionalSemicolon()
    {
        if(_token == _SC(';')) { Lex(); return; }
        if(!IsEndOfStatement()) {
            Error(_SC("end of statement expected (; or lf)"));
        }
    }
    void MoveIfCurrentTargetIsLocal() {
        SQInteger trg = _fs->TopTarget();
        if(_fs->IsLocal(trg)) {
            trg = _fs->PopTarget(); //pops the target and moves it
            _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), trg);
        }
    }
    void CleanupAfterError() {
        for (SQUnsignedInteger i=0, n=_member_constant_keys_check.size(); i<n; ++i)
            delete _member_constant_keys_check[i];
        _member_constant_keys_check.resize(0);
    }
    RootBlock *Compile(SQObjectPtr &o)
    {
        _scopedconsts.push_back();
        SQFuncState funcstate(_ss(_vm), NULL,ThrowError,this);
        funcstate._name = SQString::Create(_ss(_vm), _SC("__main__"));
        _fs = &funcstate;
        _fs->AddParameter(_fs->CreateString(_SC("this")));
        _fs->AddParameter(_fs->CreateString(_SC("vargv")));
        _fs->_varparams = true;
        _fs->_sourcename = _sourcename;
        SQInteger stacksize = _fs->GetStackSize();

        RootBlock *rootBlock = new RootBlock(_ss(_vm)->_alloc_ctx);

        if(setjmp(_errorjmp) == 0) {
            Lex();
            while(_token > 0){
                rootBlock->addStatement(parseStatement());
                if(_lex._prevtoken != _SC('}') && _lex._prevtoken != _SC(';')) OptionalSemicolon();
            }
            _fs->SetStackSize(stacksize);
            _fs->AddLineInfos(_lex._currentline, _lineinfo, true);
            _fs->AddInstruction(_OP_RETURN, 0xFF);

            assert(_member_constant_keys_check.empty());

            if (!(_fs->lang_features & LF_DISABLE_OPTIMIZER)) {
                SQOptimizer opt(*_fs);
                opt.optimize();
            }

            _fs->SetStackSize(0);
            o =_fs->BuildProto();
#ifdef _DEBUG_DUMP
            _fs->Dump(_funcproto(o));
#endif
        }
        else {
            if(_raiseerror && _ss(_vm)->_compilererrorhandler) {
                _ss(_vm)->_compilererrorhandler(_vm, _compilererror, sq_type(_sourcename) == OT_STRING?_stringval(_sourcename):_SC("unknown"),
                    _lex._currentline, _lex._currentcolumn);
            }
            _vm->_lasterror = SQString::Create(_ss(_vm), _compilererror, -1);
            CleanupAfterError();
            return NULL;
        }
        assert(_scopedconsts.size() == 1 + _num_initial_bindings);
        return rootBlock;
    }
    Block *Statements()
    {
        Block *result = new Block(_ss(_vm)->_alloc_ctx);
        while(_token != _SC('}') && _token != TK_DEFAULT && _token != TK_CASE) {
            Statement *stmt = parseStatement();
            result->addStatement(stmt);
            if(_lex._prevtoken != _SC('}') && _lex._prevtoken != _SC(';')) OptionalSemicolon();
        }
        return result;
    }
    Statement *parseStatement(bool closeframe = true)
    {
        Statement *result = NULL;
        _fs->AddLineInfos(_lex._currentline, _lineinfo);
        switch(_token){
        case _SC(';'):  Lex(); result = new EmptyStatement();         break;
        case TK_IF:     result = parseIfStatement();          break;
        case TK_WHILE:      result = parseWhileStatement();       break;
        case TK_DO:     result = parseDoWhileStatement();     break;
        case TK_FOR:        result = parseForStatement();         break;
        case TK_FOREACH:    result = parseForEachStatement();     break;
        case TK_SWITCH: result = parseSwitchStatement();      break;
        case TK_LOCAL:
        case TK_LET:
            result = parseLocalDeclStatement(_token == TK_LOCAL);
            break;
        case TK_RETURN:
        case TK_YIELD: {
            SQOpcode op;
            Expr *arg = NULL;
            if(_token == TK_RETURN) {
                op = _OP_RETURN;
            }
            else {
                op = _OP_YIELD;
                _fs->_bgenerator = true;
            }
            Lex();
            if(!IsEndOfStatement()) {
                SQInteger retexp = _fs->GetCurrentPos()+1;
                arg = Expression(SQE_RVALUE);
                if(op == _OP_RETURN && _fs->_traps > 0)
                    _fs->AddInstruction(_OP_POPTRAP, _fs->_traps, 0);
                _fs->_returnexp = retexp;
                _fs->AddInstruction(op, 1, _fs->PopTarget(),_fs->GetStackSize());
            }
            else{
                if(op == _OP_RETURN && _fs->_traps > 0)
                    _fs->AddInstruction(_OP_POPTRAP, _fs->_traps ,0);
                _fs->_returnexp = -1;
                _fs->AddInstruction(op, 0xFF,0,_fs->GetStackSize());
            }

            result = op == _OP_RETURN ? static_cast<TerminateStatement *>(new ReturnStatement(arg)) : new YieldStatement(arg);
            break;}
        case TK_BREAK:
            if(_fs->_breaktargets.size() <= 0)Error(_SC("'break' has to be in a loop block"));
            if(_fs->_breaktargets.top() > 0){
                _fs->AddInstruction(_OP_POPTRAP, _fs->_breaktargets.top(), 0);
            }
            RESOLVE_OUTERS();
            _fs->AddInstruction(_OP_JMP, 0, -1234);
            _fs->_unresolvedbreaks.push_back(_fs->GetCurrentPos());
            Lex();
            result = new BreakStatement(NULL);
            break;
        case TK_CONTINUE:
            if(_fs->_continuetargets.size() <= 0)Error(_SC("'continue' has to be in a loop block"));
            if(_fs->_continuetargets.top() > 0) {
                _fs->AddInstruction(_OP_POPTRAP, _fs->_continuetargets.top(), 0);
            }
            RESOLVE_OUTERS();
            _fs->AddInstruction(_OP_JMP, 0, -1234);
            _fs->_unresolvedcontinues.push_back(_fs->GetCurrentPos());
            Lex();
            result = new ContinueStatement(NULL);
            break;
        case TK_FUNCTION:
            if (!(_fs->lang_features & LF_NO_FUNC_DECL_SUGAR))
                result = parseFunctionStatement();
            else
                Error(_SC("Syntactic sugar for declaring functions as fields is disabled"));
            break;
        case TK_CLASS:
            if (!(_fs->lang_features & LF_NO_CLASS_DECL_SUGAR))
                result = parseClassStatement();
            else
                Error(_SC("Syntactic sugar for declaring classes as fields is disabled"));
            break;
        case TK_ENUM:
            result = parseEnumStatement(false);
            break;
        case _SC('{'):{
                BEGIN_SCOPE();
                Lex();
                result = Statements();
                Expect(_SC('}'));
                if(closeframe) {
                    END_SCOPE();
                }
                else {
                    END_SCOPE_NO_CLOSE();
                }
            }
            break;
        case TK_TRY:
            result = parseTryCatchStatement();
            break;
        case TK_THROW:
            Lex();
            result = new ThrowStatement(Expression(SQE_RVALUE));
            _fs->AddInstruction(_OP_THROW, _fs->PopTarget());
            break;
        case TK_CONST:
            result = parseConstStatement(false);
            break;
        case TK_GLOBAL:
            Lex();
            if (_token == TK_CONST)
                result = parseConstStatement(true);
            else if (_token == TK_ENUM)
                result = parseEnumStatement(true);
            else
                Error(_SC("global can be applied to const and enum only"));
            break;
        default:
            result = new ExprStatement(Expression(SQE_REGULAR));
            _fs->DiscardTarget();
            //_fs->PopTarget();
            break;
        }
        assert(result);
        _fs->SnoozeOpt();
        return result;
    }
    void EmitDerefOp(SQOpcode op)
    {
        SQInteger val = _fs->PopTarget();
        SQInteger key = _fs->PopTarget();
        SQInteger src = _fs->PopTarget();
        _fs->AddInstruction(op,_fs->PushTarget(),src,key,val);
    }
    void Emit2ArgsOP(SQOpcode op, SQInteger p3 = 0)
    {
        SQInteger p2 = _fs->PopTarget(); //src in OP_GET
        SQInteger p1 = _fs->PopTarget(); //key in OP_GET
        _fs->AddInstruction(op,_fs->PushTarget(), p1, p2, p3);
    }
    void EmitCompoundArith(SQInteger tok, SQInteger etype, SQInteger pos)
    {
        /* Generate code depending on the expression type */
        switch(etype) {
        case LOCAL:{
            SQInteger p2 = _fs->PopTarget(); //src in OP_GET
            SQInteger p1 = _fs->PopTarget(); //key in OP_GET
            _fs->PushTarget(p1);
            //EmitCompArithLocal(tok, p1, p1, p2);
            _fs->AddInstruction(ChooseArithOpByToken(tok),p1, p2, p1, 0);
            _fs->SnoozeOpt();
                   }
            break;
        case OBJECT:
        case BASE:
            {
                SQInteger val = _fs->PopTarget();
                SQInteger key = _fs->PopTarget();
                SQInteger src = _fs->PopTarget();
                /* _OP_COMPARITH mixes dest obj and source val in the arg1 */
                _fs->AddInstruction(_OP_COMPARITH, _fs->PushTarget(), (src<<16)|val, key, ChooseCompArithCharByToken(tok));
            }
            break;
        case OUTER:
            {
                SQInteger val = _fs->TopTarget();
                SQInteger tmp = _fs->PushTarget();
                _fs->AddInstruction(_OP_GETOUTER,   tmp, pos);
                _fs->AddInstruction(ChooseArithOpByToken(tok), tmp, val, tmp, 0);
                _fs->PopTarget();
                _fs->PopTarget();
                _fs->AddInstruction(_OP_SETOUTER, _fs->PushTarget(), pos, tmp);
            }
            break;
        }
    }
    Expr *parseCommaExpr(SQExpressionContext expression_context)
    {
        Expr *expr = Expression(expression_context);

        if (_token == ',') {
            CommaExpr *cm = new CommaExpr(_ss(_vm)->_alloc_ctx);
            cm->addExpression(expr);
            expr = cm;
            while (_token == ',') {
                Lex();
                cm->addExpression(Expression(expression_context));
            }
        }

        //for(;_token == ',';_fs->PopTarget(), Lex(), CommaExpr(expression_context));
        return expr;
    }
    Expr *Expression(SQExpressionContext expression_context)
    {
        SQExpressionContext saved_expression_context = _expression_context;
        _expression_context = expression_context;

        if (_ss(_vm)->_lineInfoInExpressions && _fs)
          _fs->AddLineInfos(_lex._prevtoken == _SC('\n') ? _lex._lasttokenline: _lex._currentline, _lineinfo, false);

         SQExpState es = _es;
        _es.etype     = EXPR;
        _es.epos      = -1;
        _es.donot_get = false;
        _es.literal_field = false;
        Expr *expr = LogicalNullCoalesceExp();

        if (_token == TK_INEXPR_ASSIGNMENT && (expression_context == SQE_REGULAR || expression_context == SQE_FUNCTION_ARG))
            Error(_SC(": intra-expression assignment can be used only in 'if', 'for', 'while' or 'switch'"));

        switch(_token)  {
        case _SC('='):
        case TK_INEXPR_ASSIGNMENT:
        case TK_NEWSLOT:
        case TK_MINUSEQ:
        case TK_PLUSEQ:
        case TK_MULEQ:
        case TK_DIVEQ:
        case TK_MODEQ:{
            SQInteger op = _token;
            SQInteger ds = _es.etype;
            SQInteger pos = _es.epos;
            bool literalField = _es.literal_field;
            if(ds == EXPR) Error(_SC("can't assign to expression"));
            else if(ds == BASE) Error(_SC("'base' cannot be modified"));
            else if (_es.isBinding() && _token!=TK_INEXPR_ASSIGNMENT) Error(_SC("can't assign to binding (probably declaring using 'local' was intended, but 'let' was used)"));
            Lex(); 
            Expr *e2 = Expression(SQE_RVALUE);

            switch(op){
            case TK_NEWSLOT:
                if(ds == OBJECT || ds == BASE)
                    EmitDerefOp(_OP_NEWSLOT);
                else //if _derefstate != DEREF_NO_DEREF && DEREF_FIELD so is the index of a local
                    Error(_SC("can't 'create' a local slot"));
                
                expr = new BinExpr(TO_NEWSLOT, expr, e2);

                break;

            case TK_INEXPR_ASSIGNMENT:
            case _SC('='): //ASSIGN
                if (op == _SC('='))
                    switch (expression_context)
                    {
                        case SQE_IF:
                            Error(_SC("'=' inside 'if' is forbidden"));
                            break;
                        case SQE_LOOP_CONDITION:
                            Error(_SC("'=' inside loop condition is forbidden"));
                            break;
                        case SQE_SWITCH:
                            Error(_SC("'=' inside switch is forbidden"));
                            break;
                        case SQE_FUNCTION_ARG:
                            Error(_SC("'=' inside function argument is forbidden"));
                            break;
                        case SQE_RVALUE:
                            Error(_SC("'=' inside expression is forbidden"));
                            break;
                        case SQE_REGULAR:
                            break;
                    }
                expr = new BinExpr(op == TK_INEXPR_ASSIGNMENT ? TO_INEXPR_ASSIGN : TO_ASSIGN, expr, e2);
                switch(ds) {
                case LOCAL:
                    {
                        SQInteger src = _fs->PopTarget();
                        SQInteger dst = _fs->TopTarget();
                        _fs->AddInstruction(_OP_MOVE, dst, src);
                    }
                    break;
                case OBJECT:
                case BASE:
                    EmitDerefOp(literalField ? _OP_SET_LITERAL : _OP_SET);
                    SQ_STATIC_ASSERT(_OP_DATA_NOP == 0);
                    if (literalField)
                        _fs->AddInstruction(SQOpcode(0),0,0,0,0);//hint
                    break;
                case OUTER:
                    {
                        SQInteger src = _fs->PopTarget();
                        SQInteger dst = _fs->PushTarget();
                        _fs->AddInstruction(_OP_SETOUTER, dst, pos, src);
                    }
                }
                break;
            case TK_MINUSEQ:
                expr = new BinExpr(TO_MINUSEQ, expr, e2); goto emit;
            case TK_PLUSEQ:
                expr = new BinExpr(TO_PLUSEQ, expr, e2); goto emit;
            case TK_MULEQ:
                expr = new BinExpr(TO_MULEQ, expr, e2); goto emit;
            case TK_DIVEQ:
                expr = new BinExpr(TO_DIVEQ, expr, e2); goto emit;
            case TK_MODEQ:
                expr = new BinExpr(TO_MODEQ, expr, e2); goto emit;
            emit:
                EmitCompoundArith(op, ds, pos);
                break;
            }
            }
            break;
        case _SC('?'): {
            Lex();
            _fs->AddInstruction(_OP_JZ, _fs->PopTarget());
            SQInteger jzpos = _fs->GetCurrentPos();
            SQInteger trg = _fs->PushTarget();
            Expr *ifTrue = Expression(SQE_RVALUE);
            SQInteger first_exp = _fs->PopTarget();
            if(trg != first_exp) _fs->AddInstruction(_OP_MOVE, trg, first_exp);
            SQInteger endfirstexp = _fs->GetCurrentPos();
            _fs->AddInstruction(_OP_JMP, 0, 0);
            Expect(_SC(':'));
            SQInteger jmppos = _fs->GetCurrentPos();
            Expr *ifFalse = Expression(SQE_RVALUE);
            SQInteger second_exp = _fs->PopTarget();
            if(trg != second_exp) _fs->AddInstruction(_OP_MOVE, trg, second_exp);
            _fs->SetInstructionParam(jmppos, 1, _fs->GetCurrentPos() - jmppos);
            _fs->SetInstructionParam(jzpos, 1, endfirstexp - jzpos + 1);
            _fs->SnoozeOpt();
            expr = new TerExpr(expr, ifTrue, ifFalse);
            }
            break;
        }
        _es = es;
        _expression_context = saved_expression_context;
        return expr;
    }
    template<typename T> Expr *INVOKE_EXP(T f)
    {
        SQExpState es = _es;
        _es.etype     = EXPR;
        _es.epos      = -1;
        _es.donot_get = false;
        _es.literal_field = false;
        Expr *r = (this->*f)();
        _es = es;
        return r;
    }
    template<typename T> Expr *BIN_EXP(SQOpcode op, T f, enum TreeOp top, Expr *lhs, SQInteger op3 = 0)
    {
        _expression_context = SQE_RVALUE;
        Lex();
        Expr *rhs = INVOKE_EXP(f);
        SQInteger op1 = _fs->PopTarget();
        SQInteger op2 = _fs->PopTarget();
        _fs->AddInstruction(op, _fs->PushTarget(), op1, op2, op3);
        _es.etype = EXPR;
        return new BinExpr(top, lhs, rhs);
    }
    Expr *LogicalNullCoalesceExp()
    {
        Expr *lhs = LogicalOrExp();
        for(;;) if(_token == TK_NULLCOALESCE) {
            SQInteger first_exp = _fs->PopTarget();
            SQInteger trg = _fs->PushTarget();
            _fs->AddInstruction(_OP_NULLCOALESCE, trg, 0, first_exp, 0);
            SQInteger jpos = _fs->GetCurrentPos();
            if(trg != first_exp) _fs->AddInstruction(_OP_MOVE, trg, first_exp);
            Lex(); 
            Expr *rhs = INVOKE_EXP(&SQCompiler::LogicalNullCoalesceExp);
            _fs->SnoozeOpt();
            SQInteger second_exp = _fs->PopTarget();
            if(trg != second_exp) _fs->AddInstruction(_OP_MOVE, trg, second_exp);
            _fs->SnoozeOpt();
            _fs->SetInstructionParam(jpos, 1, (_fs->GetCurrentPos() - jpos));
            _es.etype = EXPR;
            lhs = new BinExpr(TO_NULLC, lhs, rhs);
        } else return lhs;
    }
    Expr *LogicalOrExp()
    {
        Expr *lhs = LogicalAndExp();
        for(;;) if(_token == TK_OR) {
            SQInteger first_exp = _fs->PopTarget();
            SQInteger trg = _fs->PushTarget();
            _fs->AddInstruction(_OP_OR, trg, 0, first_exp, 0);
            SQInteger jpos = _fs->GetCurrentPos();
            if(trg != first_exp) _fs->AddInstruction(_OP_MOVE, trg, first_exp);
            Lex(); 
            Expr *rhs = INVOKE_EXP(&SQCompiler::LogicalOrExp);
            _fs->SnoozeOpt();
            SQInteger second_exp = _fs->PopTarget();
            if(trg != second_exp) _fs->AddInstruction(_OP_MOVE, trg, second_exp);
            _fs->SnoozeOpt();
            _fs->SetInstructionParam(jpos, 1, (_fs->GetCurrentPos() - jpos));
            _es.etype = EXPR;
            lhs = new BinExpr(TO_OROR, lhs, rhs);
        }
        else return lhs;
    }
    Expr *LogicalAndExp()
    {
        Expr *lhs = BitwiseOrExp();
        for(;;) switch(_token) {
        case TK_AND: {
            SQInteger first_exp = _fs->PopTarget();
            SQInteger trg = _fs->PushTarget();
            _fs->AddInstruction(_OP_AND, trg, 0, first_exp, 0);
            SQInteger jpos = _fs->GetCurrentPos();
            if(trg != first_exp) _fs->AddInstruction(_OP_MOVE, trg, first_exp);
            Lex(); 
            Expr *rhs = INVOKE_EXP(&SQCompiler::LogicalAndExp);
            _fs->SnoozeOpt();
            SQInteger second_exp = _fs->PopTarget();
            if(trg != second_exp) _fs->AddInstruction(_OP_MOVE, trg, second_exp);
            _fs->SnoozeOpt();
            _fs->SetInstructionParam(jpos, 1, (_fs->GetCurrentPos() - jpos));
            _es.etype = EXPR;
            lhs = new BinExpr(TO_ANDAND, lhs, rhs);
        }
        default:
            return lhs;
        }
    }
    Expr *BitwiseOrExp()
    {
        Expr *lhs = BitwiseXorExp();
        for(;;) 
            if(_token == _SC('|')) {
                return BIN_EXP(_OP_BITW, &SQCompiler::BitwiseXorExp, TO_OR, lhs, BW_OR);
            }
            else return lhs;
    }
    Expr *BitwiseXorExp()
    {
        Expr * lhs = BitwiseAndExp();
        for(;;)
            if(_token == _SC('^')) {
                lhs = BIN_EXP(_OP_BITW, &SQCompiler::BitwiseAndExp,TO_XOR, lhs, BW_XOR);
            } else return lhs;
    }
    Expr *BitwiseAndExp()
    {
        Expr *lhs = EqExp();
        for(;;)
            if(_token == _SC('&')) {
                lhs = BIN_EXP(_OP_BITW, &SQCompiler::EqExp,TO_AND, lhs, BW_AND);
            } else return lhs;
    }
    Expr *EqExp()
    {
        Expr *lhs = CompExp();
        for(;;) switch(_token) {
        case TK_EQ: lhs = BIN_EXP(_OP_EQ, &SQCompiler::CompExp, TO_EQ, lhs); break;
        case TK_NE: lhs = BIN_EXP(_OP_NE, &SQCompiler::CompExp, TO_NE, lhs); break;
        case TK_3WAYSCMP: lhs = BIN_EXP(_OP_CMP, &SQCompiler::CompExp, TO_3CMP, lhs, CMP_3W); break;
        default: return lhs;
        }
    }
    Expr *CompExp()
    {
        Expr *lhs = ShiftExp();
        for(;;) switch(_token) {
        case _SC('>'): lhs = BIN_EXP(_OP_CMP, &SQCompiler::ShiftExp,TO_GT, lhs, CMP_G); break;
        case _SC('<'): lhs = BIN_EXP(_OP_CMP, &SQCompiler::ShiftExp, TO_LT, lhs, CMP_L); break;
        case TK_GE: lhs = BIN_EXP(_OP_CMP, &SQCompiler::ShiftExp, TO_GE, lhs, CMP_GE); break;
        case TK_LE: lhs = BIN_EXP(_OP_CMP, &SQCompiler::ShiftExp, TO_LE, lhs, CMP_LE); break;
        case TK_IN: lhs = BIN_EXP(_OP_EXISTS, &SQCompiler::ShiftExp, TO_IN, lhs); break;
        case TK_INSTANCEOF: lhs = BIN_EXP(_OP_INSTANCEOF, &SQCompiler::ShiftExp, TO_INSTANCEOF, lhs); break;
        case TK_NOT: {
            Lex();
            if (_token == TK_IN) {
                lhs = BIN_EXP(_OP_EXISTS, &SQCompiler::ShiftExp, TO_IN, lhs);
                SQInteger src = _fs->PopTarget();
                _fs->AddInstruction(_OP_NOT, _fs->PushTarget(), src);
                lhs = new UnExpr(TO_NOT, lhs);
            }
            else
                Error(_SC("'in' expected "));
        }
        default: return lhs;
        }
    }
    Expr *ShiftExp()
    {
        Expr *lhs = PlusExp();
        for(;;) switch(_token) {
        case TK_USHIFTR: lhs = BIN_EXP(_OP_BITW, &SQCompiler::PlusExp, TO_USHR, lhs,BW_USHIFTR); break;
        case TK_SHIFTL: lhs = BIN_EXP(_OP_BITW, &SQCompiler::PlusExp, TO_SHL, lhs,BW_SHIFTL); break;
        case TK_SHIFTR: lhs = BIN_EXP(_OP_BITW, &SQCompiler::PlusExp, TO_SHR, lhs,BW_SHIFTR); break;
        default: return lhs;
        }
    }
    SQOpcode ChooseArithOpByToken(SQInteger tok)
    {
        switch(tok) {
            case TK_PLUSEQ: case '+': return _OP_ADD;
            case TK_MINUSEQ: case '-': return _OP_SUB;
            case TK_MULEQ: case '*': return _OP_MUL;
            case TK_DIVEQ: case '/': return _OP_DIV;
            case TK_MODEQ: case '%': return _OP_MOD;
            default: assert(0);
        }
        return _OP_ADD;
    }
    enum TreeOp ChooseArithTreeOpByToken(SQInteger tok)
    {
        switch (tok) {
        case TK_PLUSEQ: case '+': return TO_ADD;
        case TK_MINUSEQ: case '-': return TO_SUB;
        case TK_MULEQ: case '*': return TO_MUL;
        case TK_DIVEQ: case '/': return TO_DIV;
        case TK_MODEQ: case '%': return TO_MOD;
        default: assert(0);
        }
        return TO_ADD;
    }
    SQInteger ChooseCompArithCharByToken(SQInteger tok)
    {
        SQInteger oper;
        switch(tok){
        case TK_MINUSEQ: oper = '-'; break;
        case TK_PLUSEQ: oper = '+'; break;
        case TK_MULEQ: oper = '*'; break;
        case TK_DIVEQ: oper = '/'; break;
        case TK_MODEQ: oper = '%'; break;
        default: oper = 0; //shut up compiler
            assert(0); break;
        };
        return oper;
    }
    Expr *PlusExp()
    {
        Expr *lhs = MultExp();
        for(;;) switch(_token) {
        case _SC('+'): case _SC('-'):
            lhs = BIN_EXP(ChooseArithOpByToken(_token), &SQCompiler::MultExp, ChooseArithTreeOpByToken(_token), lhs); break;
        default: return lhs;
        }
    }

    Expr *MultExp()
    {
        Expr *lhs = PrefixedExpr();
        for(;;) switch(_token) {
        case _SC('*'): case _SC('/'): case _SC('%'):
            lhs = BIN_EXP(ChooseArithOpByToken(_token), &SQCompiler::PrefixedExpr, ChooseArithTreeOpByToken(_token), lhs); break;
        default: return lhs;
        }
    }
    //if 'pos' != -1 the previous variable is a local variable
    Expr *PrefixedExpr()
    {
        SQInteger pos;
        Expr *e = Factor(pos);
        bool nextIsNullable = false;
        for(;;) {
            switch(_token) {
            case _SC('.'):
            case TK_NULLGETSTR: {
                SQInteger flags = 0;
                if (_token == TK_NULLGETSTR || nextIsNullable)
                {
                    flags = OP_GET_FLAG_NO_ERROR;
                    nextIsNullable = true;
                }
                pos = -1;
                bool canBeLiteral = _es.etype!=BASE && _token == _SC('.');//todo: we can support def delegate and nullable also.
                Lex();

                SQObjectPtr constant = Expect(TK_IDENTIFIER);
                if (CanBeDefaultDelegate(constant))
                    flags |= OP_GET_FLAG_ALLOW_DEF_DELEGATE;
                _es.literal_field = canBeLiteral && !(flags & (OP_GET_FLAG_NO_ERROR|OP_GET_FLAG_ALLOW_DEF_DELEGATE));//todo: we can support def delegate and nullable also.

                e = new GetFieldExpr(e, _stringval(constant), nextIsNullable);

                SQInteger constantI = _fs->GetConstant(constant);
                _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), constantI);
                if(_es.etype==BASE) {
                    Emit2ArgsOP(_OP_GET, flags);
                    pos = _fs->TopTarget();
                    _es.etype = EXPR;
                    _es.epos   = pos;
                }
                else {
                    //todo:we can support null navigation as well
                    if(NeedGet()) {
                        if (!_es.literal_field)
                        {
                            Emit2ArgsOP(_OP_GET, flags);
                        } else {
                            Emit2ArgsOP(_OP_GET_LITERAL, flags);
                            SQ_STATIC_ASSERT(_OP_DATA_NOP == 0);
                            _fs->AddInstruction(SQOpcode(0),0,0,0,0);//hint
                        }
                    }
                    _es.etype = OBJECT;
                }
                break;
            }
            case _SC('['):
            case TK_NULLGETOBJ: {
                SQInteger flags = 0;
                if (_token == TK_NULLGETOBJ || nextIsNullable)
                {
                    flags = OP_GET_FLAG_NO_ERROR;
                    nextIsNullable = true;
                }
                if(_lex._prevtoken == _SC('\n')) Error(_SC("cannot break deref/or comma needed after [exp]=exp slot declaration"));
                _es.literal_field = false;
                Lex(); 
                Expr *key = Expression(SQE_RVALUE); 
                Expect(_SC(']'));
                e = new GetTableExpr(e, key, nextIsNullable);
                pos = -1;
                if(_es.etype==BASE) {
                    Emit2ArgsOP(_OP_GET, flags);
                    pos = _fs->TopTarget();
                    _es.etype = EXPR;
                    _es.epos   = pos;
                }
                else {
                    if(NeedGet()) {
                        Emit2ArgsOP(_OP_GET, flags);
                    }
                    _es.etype = OBJECT;
                }
                break;
            }
            case TK_MINUSMINUS:
            case TK_PLUSPLUS:
                {
                    nextIsNullable = false;
                    if(IsEndOfStatement()) return e;
                    SQInteger diff = (_token==TK_MINUSMINUS) ? -1 : 1;
                    enum TreeOp top = (_token == TK_MINUSMINUS) ? TO_PDEC : TO_PINC;
                    Lex();
                    if (_es.isBinding())
                        Error(_SC("can't '++' or '--' a binding"));
                    switch(_es.etype)
                    {
                        case EXPR: Error(_SC("can't '++' or '--' an expression")); break;
                        case BASE: Error(_SC("'base' cannot be modified")); break;
                        case OBJECT:
                            if(_es.donot_get == true)  { Error(_SC("can't '++' or '--' an expression")); break; } //mmh dor this make sense?
                            Emit2ArgsOP(_OP_PINC, diff);
                            break;
                        case LOCAL: {
                            SQInteger src = _fs->PopTarget();
                            _fs->AddInstruction(_OP_PINCL, _fs->PushTarget(), src, 0, diff);
                                    }
                            break;
                        case OUTER: {
                            SQInteger tmp1 = _fs->PushTarget();
                            SQInteger tmp2 = _fs->PushTarget();
                            _fs->AddInstruction(_OP_GETOUTER, tmp2, _es.epos);
                            _fs->AddInstruction(_OP_PINCL,    tmp1, tmp2, 0, diff);
                            _fs->AddInstruction(_OP_SETOUTER, tmp2, _es.epos, tmp2);
                            _fs->PopTarget();
                        }
                    }
                    _es.etype = EXPR;
                    e = new IncExpr(e, diff, IF_POSTFIX);
                }
                return e;
            case _SC('('):
            case TK_NULLCALL: {
                SQInteger nullcall = (_token==TK_NULLCALL || nextIsNullable);
                nextIsNullable = !!nullcall;
                CallExpr *call = new CallExpr(_ss(_vm)->_alloc_ctx, e, nullcall);
                switch(_es.etype) {
                    case OBJECT: {
                        if (!nullcall) {
                            SQInteger key     = _fs->PopTarget();  /* location of the key */
                            SQInteger table   = _fs->PopTarget();  /* location of the object */
                            SQInteger closure = _fs->PushTarget(); /* location for the closure */
                            SQInteger ttarget = _fs->PushTarget(); /* location for 'this' pointer */
                            _fs->AddInstruction(_OP_PREPCALL, closure, key, table, ttarget);
                        } else {
                            SQInteger self = _fs->GetUpTarget(1);  /* location of the object */
                            SQInteger storedSelf = _fs->PushTarget();
                            _fs->AddInstruction(_OP_MOVE, storedSelf, self);
                            _fs->PopTarget();
                            Emit2ArgsOP(_OP_GET, OP_GET_FLAG_NO_ERROR|OP_GET_FLAG_ALLOW_DEF_DELEGATE);
                            SQInteger ttarget = _fs->PushTarget();
                            _fs->AddInstruction(_OP_MOVE, ttarget, storedSelf);
                        }
                        break;
                    }
                    case BASE:
                        //Emit2ArgsOP(_OP_GET);
                        _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), 0);
                        break;
                    case OUTER:
                        _fs->AddInstruction(_OP_GETOUTER, _fs->PushTarget(), _es.epos);
                        _fs->AddInstruction(_OP_MOVE,     _fs->PushTarget(), 0);
                        break;
                    default:
                        _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), 0);
                }
                _es.etype = EXPR;
                Lex();
                FunctionCallArgs(call, false, nullcall);
                e = call;
                break;
            }
            default: return e;
            }
        }
    }
    Expr *Factor(SQInteger &pos)
    {
        if ((_token == TK_LOCAL || _token == TK_LET)
            && (_expression_context == SQE_IF || _expression_context == SQE_SWITCH || _expression_context == SQE_LOOP_CONDITION))
        {
            // TODO
            Lex();
            if (_token != TK_IDENTIFIER)
                Error(_SC("Identifier expected"));

            SQObject id = _fs->CreateString(_lex._svalue);
            CheckDuplicateLocalIdentifier(id, _SC("In-expr local"), false);
            _fs->PushLocalVariable(id, _token == TK_LOCAL);
            SQInteger res;
            Expr *x = Factor(res);
            if (_token != TK_INEXPR_ASSIGNMENT)
                Error(_SC(":= expected"));
            return x;
        }

        Expr *r = NULL;
        //_es.etype = EXPR;
        switch(_token)
        {
        case TK_STRING_LITERAL:
            r = new LiteralExpr(_lex._svalue);
            _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(_fs->CreateString(_lex._svalue,_lex._longstr.size()-1)));
            Lex();
            break;
        case TK_BASE:
            Lex();
            _fs->AddInstruction(_OP_GETBASE, _fs->PushTarget());
            _es.etype  = BASE;
            _es.epos   = _fs->TopTarget();
            pos = (_es.epos);
            return new BaseExpr(); /// TODO
        case TK_IDENTIFIER:
        case TK_CONSTRUCTOR:
        case TK_THIS:{
                bool assignable = false;
                if (_token == TK_IDENTIFIER && sq_isstring(_fs->_name)
                    && scstrcmp(_stringval(_fs->_name), _lex._svalue)==0
                    && _fs->GetLocalVariable(_fs->_name, assignable) == -1)
                {
                    _fs->AddInstruction(_OP_LOADCALLEE, _fs->PushTarget());
                    r = new CalleeExpr();
                    Lex();
                    break;
                }

                SQObject id;
                SQObject constant;

                switch(_token) {
                    case TK_IDENTIFIER:  id = _fs->CreateString(_lex._svalue); break;
                    case TK_THIS:        id = _fs->CreateString(_SC("this"),4); break;
                    case TK_CONSTRUCTOR: id = _fs->CreateString(_SC("constructor"), 11);  break;
                }

                r = new Id(id);

                if (_stringval(id) == _stringval(_fs->_name)) {
                    Error(_SC("Variable name %s conflicts with function name"), _stringval(id));
                }

                SQInteger pos = -1;
                Lex();
                if((pos = _fs->GetLocalVariable(id, assignable)) != -1) {
                    /* Handle a local variable (includes 'this') */
                    _fs->PushTarget(pos);
                    _es.etype  = LOCAL;
                    _es.epos   = pos;
                    _es.is_assignable_var = assignable;
                }

                else if((pos = _fs->GetOuterVariable(id, assignable)) != -1) {
                    /* Handle a free var */
                    if(NeedGet()) {
                        _es.epos  = _fs->PushTarget();
                        _fs->AddInstruction(_OP_GETOUTER, _es.epos, pos);
                        /* _es.etype = EXPR; already default value */
                    }
                    else {
                        _es.etype = OUTER;
                        _es.epos  = pos;
                        _es.is_assignable_var = assignable;
                    }
                }

                else if(IsConstant(id, constant)) {
                    /* Handle named constant */
                    SQObjectPtr constval = constant;
                    while (sq_type(constval) == OT_TABLE && (sq_objflags(constval) & SQOBJ_FLAG_IMMUTABLE) && _token==_SC('.')) {
                        Expect('.');
                        SQObject constid = Expect(TK_IDENTIFIER);
                        if(!_table(constval)->Get(constid, constval)) {
                            constval.Null();
                            Error(_SC("invalid enum [no '%s' field in '%s']"), _stringval(constid), _stringval(id));
                        }
                    }
                    _es.epos = _fs->PushTarget();

                    /* generate direct or literal function depending on size */
                    SQObjectType ctype = sq_type(constval);
                    switch(ctype) {
                    case OT_INTEGER:
                        //r = new LiteralExpr(_integer(constval));
                        EmitLoadConstInt(_integer(constval), _es.epos);
                        break;
                    case OT_FLOAT:
                        //r = new LiteralExpr(_float(constval));
                        EmitLoadConstFloat(_float(constval),_es.epos);
                        break;
                    case OT_BOOL:
                        //r = new LiteralExpr(static_cast<bool>(_integer(constval)));
                        _fs->AddInstruction(_OP_LOADBOOL, _es.epos, _integer(constval));
                        break;
                    default:
                        //r = new LiteralExpr(_stringval(constval));
                        _fs->AddInstruction(_OP_LOAD,_es.epos,_fs->GetConstant(constval)); break;
                    }
                    _es.etype = EXPR;
                }
                else {
                    /* Handle a non-local variable, aka a field. Push the 'this' pointer on
                    * the virtual stack (always found in offset 0, so no instruction needs to
                    * be generated), and push the key next. Generate an _OP_LOAD instruction
                    * for the latter. If we are not using the variable as a dref expr, generate
                    * the _OP_GET instruction.
                    */
                    if ((_fs->lang_features & LF_EXPLICIT_THIS)
                        && !(_fs->lang_features & LF_TOOLS_COMPILE_CHECK))
                        Error(_SC("Unknown variable [%s]"), _stringval(id));

                    _fs->PushTarget(0);
                    _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(id));
                    if(NeedGet()) {
                        Emit2ArgsOP(_OP_GET);
                    }
                    _es.etype = OBJECT;
                }
                pos = _es.epos;
                return r;
            }
            break;
        case TK_DOUBLE_COLON:  // "::"
            if (_fs->lang_features & LF_FORBID_ROOT_TABLE)
                Error(_SC("Access to root table is forbidden"));
            _fs->AddInstruction(_OP_LOADROOT, _fs->PushTarget());
            _es.etype = OBJECT;
            _token = _SC('.'); /* hack: drop into PrefixExpr, case '.'*/
            _es.epos = -1;
            pos = _es.epos;
            return new RootExpr();
        case TK_NULL:
            _fs->AddInstruction(_OP_LOADNULLS, _fs->PushTarget(),1);
            Lex();
            r = new LiteralExpr();
            break;
        case TK_INTEGER: 
            r = new LiteralExpr(_lex._nvalue);
            EmitLoadConstInt(_lex._nvalue,-1); Lex();  break;
        case TK_FLOAT: 
            r = new LiteralExpr(_lex._fvalue);
            EmitLoadConstFloat(_lex._fvalue,-1); Lex(); break;
        case TK_TRUE: case TK_FALSE:
            r = new LiteralExpr((bool)(_token == TK_TRUE));
            _fs->AddInstruction(_OP_LOADBOOL, _fs->PushTarget(),_token == TK_TRUE?1:0);
            Lex();
            break;
        case _SC('['): {
                _fs->AddInstruction(_OP_NEWOBJ, _fs->PushTarget(),0,0,NOT_ARRAY);
                SQInteger apos = _fs->GetCurrentPos(),key = 0;
                Lex();
                ArrayExpr *arr = new ArrayExpr(_ss(_vm)->_alloc_ctx);
                while(_token != _SC(']')) {
                    #if SQ_LINE_INFO_IN_STRUCTURES
                    if (key < 100)
                      _fs->AddLineInfos(_lex._currentline, false);
                    #endif
                    Expr *v = Expression(SQE_RVALUE);
                    arr->addValue(v);
                    if(_token == _SC(',')) Lex();
                    SQInteger val = _fs->PopTarget();
                    SQInteger array = _fs->TopTarget();
                    _fs->AddInstruction(_OP_APPENDARRAY, array, val, AAT_STACK);
                    key++;
                }
                _fs->SetInstructionParam(apos, 1, key);
                Lex();
                r = arr;
            }
            break;
        case _SC('{'): {
            _fs->AddInstruction(_OP_NEWOBJ, _fs->PushTarget(), 0, 0, NOT_TABLE);
            Lex();
            // TODO
            TableDecl *t = new TableDecl(_ss(_vm)->_alloc_ctx);
            ParseTableOrClass(t, _SC(','), _SC('}'));
            r = new DeclExpr(t);
            break;
        }
        case TK_FUNCTION: 
            r = FunctionExp(_token);
            break;
        case _SC('@'):
            r = FunctionExp(_token,true);
            break;
        case TK_CLASS: {
            Lex();
            Decl *classDecl = ClassExp(NULL);
            classDecl->setContext(DC_EXPR);
            r = new DeclExpr(classDecl);
            break;
        }
        case _SC('-'):
            Lex();
            switch(_token) {
            case TK_INTEGER: r = new LiteralExpr(-_lex._nvalue); EmitLoadConstInt(-_lex._nvalue, -1); Lex(); break;
            case TK_FLOAT: r = new LiteralExpr(-_lex._fvalue); EmitLoadConstFloat(-_lex._fvalue,-1); Lex(); break;
            default: r = UnaryOP(_OP_NEG);
            }
            break;
        case _SC('!'): Lex(); r = UnaryOP(_OP_NOT); break;
        case _SC('~'):
            Lex();
            if(_token == TK_INTEGER)  { 
                r = new LiteralExpr(~_lex._nvalue);
                EmitLoadConstInt(~_lex._nvalue,-1); Lex(); break;
            }
            r = UnaryOP(_OP_BWNOT);
            break;
        case TK_TYPEOF : Lex(); r = UnaryOP(_OP_TYPEOF); break;
        case TK_RESUME : Lex(); r = UnaryOP(_OP_RESUME); break;
        case TK_CLONE : Lex(); r = UnaryOP(_OP_CLONE); break;
        //case TK_RAWCALL: Lex(); Expect('('); FunctionCallArgs(true, false); break; // drop it
        case TK_MINUSMINUS :
        case TK_PLUSPLUS :
            r = PrefixIncDec(_token);
            break;
        case TK_DELETE : r = DeleteExpr(); break;
        case _SC('('): 
            Lex(); 
            r = new UnExpr(TO_PAREN, Expression(_expression_context));
            Expect(_SC(')'));
            break;
        case TK___LINE__: EmitLoadConstInt(_lex._currentline,-1); Lex(); break;
        case TK___FILE__: _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(_sourcename)); Lex(); break;
        default: Error(_SC("expression expected"));
        }
        _es.etype = EXPR;
        pos = -1;
        return r;
    }
    void EmitLoadConstInt(SQInteger value,SQInteger target)
    {
        if(target < 0) {
            target = _fs->PushTarget();
        }
        if(value <= INT_MAX && value > INT_MIN) { //does it fit in 32 bits?
            _fs->AddInstruction(_OP_LOADINT, target,value);
        }
        else {
            _fs->AddInstruction(_OP_LOAD, target, _fs->GetNumericConstant(value));
        }
    }
    void EmitLoadConstFloat(SQFloat value,SQInteger target)
    {
        if(target < 0) {
            target = _fs->PushTarget();
        }
        if(sizeof(SQFloat) == sizeof(SQInt32)) {
            _fs->AddInstruction(_OP_LOADFLOAT, target,*((SQInt32 *)&value));
        }
        else {
            _fs->AddInstruction(_OP_LOAD, target, _fs->GetNumericConstant(value));
        }
    }

    enum TreeOp unaryOp2TreeOp(SQOpcode op) {
        switch (op)
        {
        case _OP_NEG: return TO_NEG;
        case _OP_NOT: return TO_NOT;
        case _OP_BWNOT: return TO_BNOT;
        case _OP_TYPEOF: return TO_TYPEOF;
        case _OP_RESUME: return TO_RESUME;
        case _OP_CLONE: return TO_CLONE;
        default:
            assert(0);
            return (TreeOp)0;
        }
    }

    Expr *UnaryOP(SQOpcode op)
    {
        Expr *arg = PrefixedExpr();
        if (_fs->_targetstack.size() == 0)
            Error(_SC("cannot evaluate unary-op"));
        SQInteger src = _fs->PopTarget();
        _fs->AddInstruction(op, _fs->PushTarget(), src);
        return new UnExpr(unaryOp2TreeOp(op), arg);

    }
    bool NeedGet()
    {
        switch(_token) {
        case _SC('('): case TK_NULLCALL:
            return false;
        case _SC('='): case TK_NEWSLOT: case TK_MODEQ: case TK_MULEQ:
        case TK_DIVEQ: case TK_MINUSEQ: case TK_PLUSEQ:
            if (_expression_context != SQE_REGULAR)
                Error("can't assign to an expression or inside return/yield");
            return false;
        case TK_PLUSPLUS: case TK_MINUSMINUS:
            if (!IsEndOfStatement()) {
                return false;
            }
        break;
        }
        return (!_es.donot_get || ( _es.donot_get && (_token == _SC('.') || _token == _SC('['))));
    }
    void FunctionCallArgs(CallExpr *call, bool rawcall, bool nullcall)
    {
        SQInteger nargs = 1;//this
         while(_token != _SC(')')) {
             call->addArgument(Expression(SQE_FUNCTION_ARG));
             MoveIfCurrentTargetIsLocal();
             nargs++;
             if(_token == _SC(',')){
                 Lex();
             }
         }
         Lex();
         if (rawcall) {
             if (nargs < 3) Error(_SC("rawcall requires at least 2 parameters (callee and this)"));
             nargs -= 2; //removes callee and this from count
         }
         for(SQInteger i = 0; i < (nargs - 1); i++) _fs->PopTarget();
         SQInteger stackbase = _fs->PopTarget();
         SQInteger closure = _fs->PopTarget();
         SQInteger target = _fs->PushTarget();
         assert(target >= -1);
         assert(target < 255);
         _fs->AddInstruction(nullcall ? _OP_NULLCALL : _OP_CALL, target, closure, stackbase, nargs);
    }
    bool CheckMemberUniqueness(sqvector<SQObject> &vec, SQObject &obj) {
        for (SQUnsignedInteger i=0, n=vec.size(); i<n; ++i) {
            if (vec[i]._type == obj._type && vec[i]._unVal.raw == obj._unVal.raw) {
                if (sq_isstring(obj))
                    Error(_SC("duplicate key '%s'"), sq_objtostring(&obj));
                else
                    Error(_SC("duplicate key"));
                return false;
            }
        }
        vec.push_back(obj);
        return true;
    }
    void ParseTableOrClass(TableDecl *decl, SQInteger separator,SQInteger terminator)
    {
        SQInteger tpos = _fs->GetCurrentPos(),nkeys = 0;
        sqvector<SQObject> *memberConstantKeys = new sqvector<SQObject>(_fs->_sharedstate->_alloc_ctx);
        _member_constant_keys_check.push_back(memberConstantKeys);
        NewObjectType otype = separator==_SC(',') ? NOT_TABLE : NOT_CLASS;
        while(_token != terminator) {
            #if SQ_LINE_INFO_IN_STRUCTURES
            if (nkeys < 100)
              _fs->AddLineInfos(_lex._currentline, false);
            #endif
            bool isstatic = false;
            //check if is an static
            if(otype == NOT_CLASS) {
                if(_token == TK_STATIC) {
                    isstatic = true;
                    Lex();
                }
            }
            switch(_token) {
            case TK_FUNCTION:
            case TK_CONSTRUCTOR:{
                SQInteger tk = _token;
                Lex();
                SQObject id = tk == TK_FUNCTION ? Expect(TK_IDENTIFIER) : _fs->CreateString(_SC("constructor"));
                SQChar *lit = _stringval(id);
                LiteralExpr *key = new LiteralExpr(lit);
                CheckMemberUniqueness(*memberConstantKeys, id);
                Expect(_SC('('));
                _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(id));
                FunctionDecl *f = CreateFunction(id, false, tk == TK_CONSTRUCTOR);
                _fs->AddInstruction(_OP_CLOSURE, _fs->PushTarget(), _fs->_functions.size() - 1, 0);
                decl->addMember(key, f, isstatic);
            }
            break;
            case _SC('['): {
                Lex();

                SQObjectPtr firstId;
                SQUnsignedInteger prevInstrSize = _fs->_instructions.size();
                if (_token == TK_STRING_LITERAL)
                    firstId = _fs->CreateString(_lex._svalue,_lex._longstr.size()-1);
                else if (_token == TK_INTEGER)
                    firstId = SQObjectPtr(_lex._nvalue);
                Expr *key = Expression(SQE_RVALUE);
                if (!sq_isnull(firstId) && _fs->_instructions.size() == prevInstrSize+1) {
                    unsigned char op = _fs->_instructions.back().op;
                    if (op == _OP_LOAD || op == _OP_LOADINT)
                        CheckMemberUniqueness(*memberConstantKeys, firstId);
                }
                Expect(_SC(']'));
                Expect(_SC('='));
                Expr *value = Expression(SQE_RVALUE);
                decl->addMember(key, value, isstatic);
                break;
            }
            case TK_STRING_LITERAL: //JSON
                if(otype == NOT_TABLE) { //only works for tables
                    SQObject id = Expect(TK_STRING_LITERAL);
                    SQChar *lit = _stringval(id);
                    LiteralExpr *key = new LiteralExpr(lit);
                    CheckMemberUniqueness(*memberConstantKeys, id);
                    _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(id));
                    Expect(_SC(':')); 
                    Expr *expr = Expression(SQE_RVALUE);
                    decl->addMember(key, expr, isstatic);
                    break;
                }  //-V796
            default : {
                SQObject id = Expect(TK_IDENTIFIER);
                SQChar *lit = _stringval(id);
                LiteralExpr *key = new LiteralExpr(lit);
                CheckMemberUniqueness(*memberConstantKeys, id);
                _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(id));
                // TODO:
                if ((otype == NOT_TABLE) &&
                    (_token == TK_IDENTIFIER || _token == separator || _token == terminator || _token == _SC('[')
                        || _token == TK_FUNCTION)) {
                    SQObject constant;
                    SQInteger pos = -1;
                    bool assignable = false;
                    Id *idExpr = new Id(id);
                    decl->addMember(key, idExpr, isstatic);
                    if((pos = _fs->GetLocalVariable(id, assignable)) != -1)
                        _fs->PushTarget(pos);
                    else if((pos = _fs->GetOuterVariable(id,assignable)) != -1)
                        _fs->AddInstruction(_OP_GETOUTER, _fs->PushTarget(), pos);
                    else if(IsConstant(id, constant))
                        _fs->AddInstruction(_OP_LOAD,_fs->PushTarget(),_fs->GetConstant(constant));
                    else
                        Error(_SC("Invalid slot initializer '%s' - no such variable/constant or incorrect expression"), _stringval(id));
                }
                else {
                    Expect(_SC('=')); 
                    Expr *expr = Expression(SQE_RVALUE);
                    decl->addMember(key, expr, isstatic);
                }
            }
            }
            if(_token == separator) Lex();//optional comma/semicolon
            nkeys++;
            SQInteger val = _fs->PopTarget();
            SQInteger key = _fs->PopTarget();
            unsigned char flags = isstatic ? NEW_SLOT_STATIC_FLAG : 0;
            SQInteger table = _fs->TopTarget(); //<<BECAUSE OF THIS NO COMMON EMIT FUNC IS POSSIBLE
            if (otype == NOT_TABLE) {
                _fs->AddInstruction(_OP_NEWSLOT, 0xFF, table, key, val);
            }
            else {
                _fs->AddInstruction(_OP_NEWSLOTA, flags, table, key, val); //this for classes only as it invokes _newmember
            }
        }
        if(otype==NOT_TABLE)
            _fs->SetInstructionParam(tpos, 1, nkeys);
        Lex();
        delete memberConstantKeys;
        _member_constant_keys_check.pop_back();
    }
    void CheckDuplicateLocalIdentifier(const SQObject &name, const SQChar *desc, bool ignore_global_consts)
    {
        bool assignable = false;
        if (_fs->GetLocalVariable(name, assignable) >= 0)
            Error(_SC("%s name '%s' conflicts with existing local variable"), desc, _string(name)->_val);
        if (_stringval(name) == _stringval(_fs->_name))
            Error(_SC("%s name '%s' conflicts with function name"), desc, _stringval(name));

        SQObject constant;
        if (ignore_global_consts ? IsLocalConstant(name, constant) : IsConstant(name, constant))
            Error(_SC("%s name '%s' conflicts with existing constant/enum/import"), desc, _stringval(name));
    }
    Decl *parseLocalDeclStatement(bool assignable)
    {
        SQObject varname;
        Lex();
        if( _token == TK_FUNCTION) {
            Lex();
            varname = Expect(TK_IDENTIFIER);
            CheckDuplicateLocalIdentifier(varname, _SC("Function"), false);
            Expect(_SC('('));
            FunctionDecl *f = CreateFunction(varname,false);
            _fs->AddInstruction(_OP_CLOSURE, _fs->PushTarget(), _fs->_functions.size() - 1, 0);
            _fs->PopTarget();
            _fs->PushLocalVariable(varname, assignable);
            f->setContext(DC_LOCAL);
            return new VarDecl(new Id(varname), new DeclExpr(f), assignable);
            //return f;
        } else if (_token == TK_CLASS) {
            Lex();
            varname = Expect(TK_IDENTIFIER);
            CheckDuplicateLocalIdentifier(varname, _SC("Class"), false);
            ClassDecl *c = ClassExp(NULL);
            c->setContext(DC_LOCAL);
            _fs->PopTarget();
            _fs->PushLocalVariable(varname, assignable);
            return new VarDecl(new Id(varname), new DeclExpr(c), assignable);
            //return c;
        }

        DeclGroup *decls = NULL;
        DesctructionDecl *dd = NULL;
        Decl *decl = NULL;
        SQInteger destructurer = 0;
        if (_token == _SC('{') || _token == _SC('[')) {
            destructurer = _token;
            Lex();
            decls = dd = new DesctructionDecl(_ss(_vm)->_alloc_ctx, destructurer == _SC('{') ? DT_TABLE : DT_ARRAY);
        }

        sqvector<SQInteger> targets(_ss(_vm)->_alloc_ctx);
        sqvector<SQInteger> flags(_ss(_vm)->_alloc_ctx);
        SQObjectPtrVec names(_ss(_vm)->_alloc_ctx);


        do {
            varname = Expect(TK_IDENTIFIER);
            CheckDuplicateLocalIdentifier(varname, assignable ? _SC("Local variable") : _SC("Named binding"), false);
            VarDecl *cur = NULL;
            if(_token == _SC('=')) {
                Lex(); 
                Expr *expr = Expression(SQE_REGULAR);
                SQInteger src = _fs->PopTarget();
                SQInteger dest = _fs->PushTarget();
                if(dest != src) _fs->AddInstruction(_OP_MOVE, dest, src);
                flags.push_back(OP_GET_FLAG_NO_ERROR | OP_GET_FLAG_KEEP_VAL);
                cur = new VarDecl(new Id(varname), expr, assignable);
            }
            else {
                if (!assignable && !destructurer)
                    Error(_SC("Binding '%s' must be initialized"), _stringval(varname));
                _fs->AddInstruction(_OP_LOADNULLS, _fs->PushTarget(),1);
                flags.push_back(0);
                cur = new VarDecl(new Id(varname), NULL, assignable);
                //assert(dd);
                //dd->addName(new Id(varname));
            }
            targets.push_back(_fs->PopTarget());
            _fs->PushLocalVariable(varname, assignable);
            names.push_back(varname);

            if (decls) {
                decls->addDeclaration(cur);
            } else if (decl) {
                decls = new DeclGroup(_ss(_vm)->_alloc_ctx);
                decls->addDeclaration(static_cast<VarDecl *>(decl));
                decls->addDeclaration(cur);
                decl = decls;
            } else {
                decl = cur;
            }

            if (destructurer) {
                if (_token == _SC(',')) {
                    Lex();
                    if (_token == _SC(']') || _token == _SC('}'))
                        break;
                }
                else if (_token == TK_IDENTIFIER)
                    continue;
                else
                    break;
            }
            else {
                if (_token == _SC(','))
                    Lex();
                else
                    break;
            }
        } while(1);

        if (destructurer) {
            Expect(destructurer==_SC('[') ? _SC(']') : _SC('}'));
            Expect(_SC('='));
            dd->setExpression(Expression(SQE_RVALUE));
            SQInteger src = _fs->TopTarget();
            SQInteger key_pos = _fs->PushTarget();
            if (destructurer == _SC('[')) {
                for (SQUnsignedInteger i=0; i<targets.size(); ++i) {
                    EmitLoadConstInt(i, key_pos);
                    _fs->AddInstruction(_OP_GET, targets[i], src, key_pos, flags[i]);
                }
            }
            else {
                for (SQUnsignedInteger i=0; i<targets.size(); ++i) {
                    _fs->AddInstruction(_OP_LOAD, key_pos, _fs->GetConstant(names[i]));
                    _fs->AddInstruction(_OP_GET, targets[i], src, key_pos, flags[i]);
                }
            }
            _fs->PopTarget();
            _fs->PopTarget();
            return dd;
        } else {
            return decls ? static_cast<Decl*>(decls) : decl;
        }
    }
    Statement *IfBlock()
    {
        Statement *stmt = NULL;
        if (_token == _SC('{'))
        {
            BEGIN_SCOPE();
            Lex();
            stmt = Statements();
            Expect(_SC('}'));
            END_SCOPE();
        }
        else {
            BEGIN_SCOPE();
            stmt = parseStatement();
            Block *block = new Block(_ss(_vm)->_alloc_ctx);
            block->addStatement(stmt);
            stmt = block;
            if (_lex._prevtoken != _SC('}') && _lex._prevtoken != _SC(';')) OptionalSemicolon();
            END_SCOPE();
        }

        return stmt;
    }
    IfStatement *parseIfStatement()
    {
        BEGIN_SCOPE();

        SQInteger jmppos;
        bool haselse = false;

        Lex();
        Expect(_SC('('));
        Expr *cond = Expression(SQE_IF);
        Expect(_SC(')'));
        
        _fs->AddInstruction(_OP_JZ, _fs->PopTarget());
        SQInteger jnepos = _fs->GetCurrentPos();



        Statement *thenB = IfBlock();
        //
        /*static int n = 0;
        if (_token != _SC('}') && _token != TK_ELSE) {
            printf("IF %d-----------------------!!!!!!!!!\n", n);
            if (n == 5)
            {
                printf("asd");
            }
            n++;
            //OptionalSemicolon();
        }*/


        SQInteger endifblock = _fs->GetCurrentPos();
        Statement *elseB = NULL;
        if(_token == TK_ELSE){
            haselse = true;
            //BEGIN_SCOPE();
            _fs->AddInstruction(_OP_JMP);
            jmppos = _fs->GetCurrentPos();
            Lex();
            //Statement(); if(_lex._prevtoken != _SC('}')) OptionalSemicolon();
            elseB = IfBlock();
            //END_SCOPE();
            _fs->SetInstructionParam(jmppos, 1, _fs->GetCurrentPos() - jmppos);
        }
        _fs->SetInstructionParam(jnepos, 1, endifblock - jnepos + (haselse?1:0));
        END_SCOPE();
        return new IfStatement(cond, thenB, elseB);
    }
    WhileStatement *parseWhileStatement()
    {
        Expr *cond = NULL;
        Statement *body = NULL;
        BEGIN_SCOPE();
        {

        SQInteger jzpos, jmppos;
        jmppos = _fs->GetCurrentPos();
        Lex();
        Expect(_SC('(')); 
        cond = Expression(SQE_LOOP_CONDITION);
        Expect(_SC(')'));

        BEGIN_BREAKBLE_BLOCK();
        _fs->AddInstruction(_OP_JZ, _fs->PopTarget());
        jzpos = _fs->GetCurrentPos();
        BEGIN_SCOPE();

        body = parseStatement();

        END_SCOPE();
        _fs->AddInstruction(_OP_JMP, 0, jmppos - _fs->GetCurrentPos() - 1);
        _fs->SetInstructionParam(jzpos, 1, _fs->GetCurrentPos() - jzpos);

        END_BREAKBLE_BLOCK(jmppos);

        }
        END_SCOPE();

        return new WhileStatement(cond, body);
    }
    DoWhileStatement *parseDoWhileStatement()
    {
        Expr *cond = NULL;
        Statement *body = NULL;
        BEGIN_SCOPE();
        {

        Lex();
        SQInteger jmptrg = _fs->GetCurrentPos();
        BEGIN_BREAKBLE_BLOCK()
        BEGIN_SCOPE();
        body = parseStatement();
        END_SCOPE();
        Expect(TK_WHILE);
        SQInteger continuetrg = _fs->GetCurrentPos();
        Expect(_SC('('));
        cond = Expression(SQE_LOOP_CONDITION);
        Expect(_SC(')'));
        _fs->AddInstruction(_OP_JZ, _fs->PopTarget(), 1);
        _fs->AddInstruction(_OP_JMP, 0, jmptrg - _fs->GetCurrentPos() - 1);
        END_BREAKBLE_BLOCK(continuetrg);

        }
        END_SCOPE();
        return new DoWhileStatement(body, cond);
    }
    ForStatement *parseForStatement()
    {
        Node *init = NULL;
        Expr *cond = NULL, *mod = NULL;
        Statement *body = NULL;
        Lex();
        BEGIN_SCOPE();
        Expect(_SC('('));
        if(_token == TK_LOCAL) init = parseLocalDeclStatement(true);
        else if(_token != _SC(';')){
            // TODO:
            init = parseCommaExpr(SQE_REGULAR);
            _fs->PopTarget();
        }
        Expect(_SC(';'));
        _fs->SnoozeOpt();
        SQInteger jmppos = _fs->GetCurrentPos();
        SQInteger jzpos = -1;
        if(_token != _SC(';')) { cond = Expression(SQE_LOOP_CONDITION); _fs->AddInstruction(_OP_JZ, _fs->PopTarget()); jzpos = _fs->GetCurrentPos(); }
        Expect(_SC(';'));
        _fs->SnoozeOpt();
        SQInteger expstart = _fs->GetCurrentPos() + 1;
        if(_token != _SC(')')) {
            mod = parseCommaExpr(SQE_REGULAR);
            _fs->PopTarget();
        }
        Expect(_SC(')'));
        _fs->SnoozeOpt();
        SQInteger expend = _fs->GetCurrentPos();
        SQInteger expsize = (expend - expstart) + 1;
        SQInstructionVec exp(_fs->_sharedstate->_alloc_ctx);
        if(expsize > 0) {
            for(SQInteger i = 0; i < expsize; i++)
                exp.push_back(_fs->GetInstruction(expstart + i));
            _fs->PopInstructions(expsize);
        }
        BEGIN_BREAKBLE_BLOCK()
        body = parseStatement();
        SQInteger continuetrg = _fs->GetCurrentPos();
        if(expsize > 0) {
            for(SQInteger i = 0; i < expsize; i++)
                _fs->AddInstruction(exp[i]);
        }
        _fs->AddInstruction(_OP_JMP, 0, jmppos - _fs->GetCurrentPos() - 1, 0);
        if(jzpos>  0) _fs->SetInstructionParam(jzpos, 1, _fs->GetCurrentPos() - jzpos);

        END_BREAKBLE_BLOCK(continuetrg);

        END_SCOPE();

        return new ForStatement(init, cond, mod, body);
    }

    ForeachStatement *parseForEachStatement()
    {
        Id *idx = NULL, *val = NULL;
        Expr *contnr = NULL;
        Statement *body = NULL;
        SQObject idxname, valname;
        Lex(); Expect(_SC('(')); valname = Expect(TK_IDENTIFIER);
        CheckDuplicateLocalIdentifier(valname, _SC("Iterator"), false);

        if(_token == _SC(',')) {
            idxname = valname;
            Lex(); valname = Expect(TK_IDENTIFIER);
            CheckDuplicateLocalIdentifier(valname, _SC("Iterator"), false);
            if (_stringval(idxname) == _stringval(valname))
                Error(_SC("foreach() key and value names are the same: %s"), _stringval(valname));
        }
        else{
            idxname = _fs->CreateString(_SC("@INDEX@"));
        }
        Expect(TK_IN);

        //save the stack size
        BEGIN_SCOPE();
        //put the table in the stack(evaluate the table expression)
        contnr = Expression(SQE_RVALUE);
        Expect(_SC(')'));
        SQInteger container = _fs->TopTarget();
        //push the index local var
        SQInteger indexpos = _fs->PushLocalVariable(idxname, false);
        _fs->AddInstruction(_OP_LOADNULLS, indexpos,1);
        //push the value local var
        SQInteger valuepos = _fs->PushLocalVariable(valname, false);
        _fs->AddInstruction(_OP_LOADNULLS, valuepos,1);
        //push reference index
        SQInteger itrpos = _fs->PushLocalVariable(_fs->CreateString(_SC("@ITERATOR@")), false); //use invalid id to make it inaccessible
        _fs->AddInstruction(_OP_LOADNULLS, itrpos,1);
        SQInteger jmppos = _fs->GetCurrentPos();
        _fs->AddInstruction(_OP_FOREACH, container, 0, indexpos);
        SQInteger foreachpos = _fs->GetCurrentPos();
        _fs->AddInstruction(_OP_POSTFOREACH, container, 0, indexpos);
        //generate the statement code
        BEGIN_BREAKBLE_BLOCK()
        body = parseStatement();
        _fs->AddInstruction(_OP_JMP, 0, jmppos - _fs->GetCurrentPos() - 1);
        _fs->SetInstructionParam(foreachpos, 1, _fs->GetCurrentPos() - foreachpos);
        _fs->SetInstructionParam(foreachpos + 1, 1, _fs->GetCurrentPos() - foreachpos);
        END_BREAKBLE_BLOCK(foreachpos - 1);
        //restore the local variable stack(remove index,val and ref idx)
        _fs->PopTarget();
        END_SCOPE();

        return new ForeachStatement(new Id(idxname), new Id(valname), contnr, body);
    }
    SwitchStatement *parseSwitchStatement()
    {
        SwitchStatement *switchStmt = NULL;
        BEGIN_SCOPE();
        Lex();
        Expect(_SC('('));
        switchStmt = new SwitchStatement(_ss(_vm)->_alloc_ctx, Expression(SQE_SWITCH));
        Expect(_SC(')'));
        Expect(_SC('{'));
        SQInteger expr = _fs->TopTarget();
        bool bfirst = true;
        SQInteger tonextcondjmp = -1;
        SQInteger skipcondjmp = -1;
        SQInteger __nbreaks__ = _fs->_unresolvedbreaks.size();
        _fs->_breaktargets.push_back(0);
        _fs->_blockstacksizes.push_back(_scope.stacksize);
        while(_token == TK_CASE) {
            if(!bfirst) {
                _fs->AddInstruction(_OP_JMP, 0, 0);
                skipcondjmp = _fs->GetCurrentPos();
                _fs->SetInstructionParam(tonextcondjmp, 1, _fs->GetCurrentPos() - tonextcondjmp);
            }
            //condition
            Lex(); 
            Expr *cond = Expression(SQE_RVALUE);
            Expect(_SC(':'));
            SQInteger trg = _fs->PopTarget();
            SQInteger eqtarget = trg;
            bool local = _fs->IsLocal(trg);
            if(local) {
                eqtarget = _fs->PushTarget(); //we need to allocate a extra reg
            }
            _fs->AddInstruction(_OP_EQ, eqtarget, trg, expr);
            _fs->AddInstruction(_OP_JZ, eqtarget, 0);
            if(local) {
                _fs->PopTarget();
            }

            //end condition
            if(skipcondjmp != -1) {
                _fs->SetInstructionParam(skipcondjmp, 1, (_fs->GetCurrentPos() - skipcondjmp));
            }
            tonextcondjmp = _fs->GetCurrentPos();
            BEGIN_SCOPE();
            Statement *stmt = Statements();
            switchStmt->addCases(cond, stmt);
            END_SCOPE();
            bfirst = false;
        }
        if(tonextcondjmp != -1)
            _fs->SetInstructionParam(tonextcondjmp, 1, _fs->GetCurrentPos() - tonextcondjmp);
        if(_token == TK_DEFAULT) {
            Lex(); Expect(_SC(':'));
            BEGIN_SCOPE();
            switchStmt->addDefault(Statements());
            END_SCOPE();
        }
        Expect(_SC('}'));
        _fs->PopTarget();
        __nbreaks__ = _fs->_unresolvedbreaks.size() - __nbreaks__;
        if(__nbreaks__ > 0)ResolveBreaks(_fs, __nbreaks__);
        _fs->_breaktargets.pop_back();
        _fs->_blockstacksizes.pop_back();
        END_SCOPE();

        return switchStmt;
    }
    FunctionDecl *parseFunctionStatement()
    {
        SQObject id;
        Lex(); id = Expect(TK_IDENTIFIER);
        _fs->PushTarget(0);
        _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(id));
        Expect(_SC('('));
        FunctionDecl *f = CreateFunction(id);
        _fs->AddInstruction(_OP_CLOSURE, _fs->PushTarget(), _fs->_functions.size() - 1, 0);
        EmitDerefOp(_OP_NEWSLOT);
        _fs->PopTarget();
        return f;
    }
    ClassDecl *parseClassStatement()
    {
        SQExpState es;
        Lex();
        es = _es;
        _es.donot_get = true;
        Expr *key = PrefixedExpr();
        ClassDecl *decl = NULL;
        if(_es.etype == EXPR) {
            Error(_SC("invalid class name"));
        }
        else if(_es.etype == OBJECT || _es.etype == BASE) {
            decl = ClassExp(key);
            decl->setContext(DC_SLOT);
            EmitDerefOp(_OP_NEWSLOT);
            _fs->PopTarget();
        }
        else {
            Error(_SC("cannot create a class in a local with the syntax(class <local>)"));
        }
        _es = es;

        return decl;
    }
    SQObject ExpectScalar()
    {
        SQObject val;
        val._type = OT_NULL; val._unVal.nInteger = 0; //shut up GCC 4.x
        val._flags = 0;
        switch(_token) {
            case TK_INTEGER:
                val._type = OT_INTEGER;
                val._unVal.nInteger = _lex._nvalue;
                break;
            case TK_FLOAT:
                val._type = OT_FLOAT;
                val._unVal.fFloat = _lex._fvalue;
                break;
            case TK_STRING_LITERAL:
                val = _fs->CreateString(_lex._svalue,_lex._longstr.size()-1);
                break;
            case TK_TRUE:
            case TK_FALSE:
                val._type = OT_BOOL;
                val._unVal.nInteger = _token == TK_TRUE ? 1 : 0;
                break;
            case '-':
                Lex();
                switch(_token)
                {
                case TK_INTEGER:
                    val._type = OT_INTEGER;
                    val._unVal.nInteger = -_lex._nvalue;
                break;
                case TK_FLOAT:
                    val._type = OT_FLOAT;
                    val._unVal.fFloat = -_lex._fvalue;
                break;
                default:
                    Error(_SC("scalar expected : integer, float"));
                }
                break;
            default:
                Error(_SC("scalar expected : integer, float, or string"));
        }
        Lex();
        return val;
    }

    SQTable* GetScopedConstsTable()
    {
        assert(!_scopedconsts.empty());
        SQObjectPtr &consts = _scopedconsts.top();
        if (sq_type(consts) != OT_TABLE)
            consts = SQTable::Create(_ss(_vm), 0);
        return _table(consts);
    }

    ConstDecl *parseConstStatement(bool global)
    {
        Lex();
        SQObject id = Expect(TK_IDENTIFIER);
        bool ignoreGlobalConst = global && !(_fs->lang_features & LF_FORBID_GLOBAL_CONST_REWRITE);
        CheckDuplicateLocalIdentifier(id, _SC("Constant"), ignoreGlobalConst);

        Expect('=');
        SQObject val = ExpectScalar();
        OptionalSemicolon();

        SQTable *enums = global ? _table(_ss(_vm)->_consts) : GetScopedConstsTable();

        SQObjectPtr strongid = id;
        enums->NewSlot(strongid,SQObjectPtr(val));
        strongid.Null();

        return new ConstDecl(new Id(id), val, global);
    }

    EnumDecl *parseEnumStatement(bool global)
    {
        Lex();
        SQObject id = Expect(TK_IDENTIFIER);
        bool ignoreGlobalConst = global && !(_fs->lang_features & LF_FORBID_GLOBAL_CONST_REWRITE);
        CheckDuplicateLocalIdentifier(id, _SC("Enum"), ignoreGlobalConst);

        EnumDecl *decl = new EnumDecl(_ss(_vm)->_alloc_ctx, new Id(id), global);

        Expect(_SC('{'));

        SQObject table = _fs->CreateTable();
        table._flags = SQOBJ_FLAG_IMMUTABLE;
        SQInteger nval = 0;
        while(_token != _SC('}')) {
            SQObject key = Expect(TK_IDENTIFIER);
            SQObject val;
            if(_token == _SC('=')) {
                Lex();
                val = ExpectScalar();
            }
            else {
                val._type = OT_INTEGER;
                val._unVal.nInteger = nval++;
                val._flags = 0;
            }

            decl->addConst(new Id(key), val);

            _table(table)->NewSlot(SQObjectPtr(key),SQObjectPtr(val));
            if(_token == ',') Lex();
        }

        SQTable *enums = global ? _table(_ss(_vm)->_consts) : GetScopedConstsTable();

        SQObjectPtr strongid = id;
        enums->NewSlot(SQObjectPtr(strongid),SQObjectPtr(table));
        strongid.Null();
        Lex();

        return decl;
    }
    TryStatement *parseTryCatchStatement()
    {
        SQObject exid;
        Statement *t = NULL, *c = NULL;
        Lex();
        _fs->AddInstruction(_OP_PUSHTRAP,0,0);
        _fs->_traps++;
        if(_fs->_breaktargets.size()) _fs->_breaktargets.top()++;
        if(_fs->_continuetargets.size()) _fs->_continuetargets.top()++;
        SQInteger trappos = _fs->GetCurrentPos();
        {
            BEGIN_SCOPE();
            t = parseStatement();
            END_SCOPE();
        }
        _fs->_traps--;
        _fs->AddInstruction(_OP_POPTRAP, 1, 0);
        if(_fs->_breaktargets.size()) _fs->_breaktargets.top()--;
        if(_fs->_continuetargets.size()) _fs->_continuetargets.top()--;
        _fs->AddInstruction(_OP_JMP, 0, 0);
        SQInteger jmppos = _fs->GetCurrentPos();
        _fs->SetInstructionParam(trappos, 1, (_fs->GetCurrentPos() - trappos));
        Expect(TK_CATCH); Expect(_SC('(')); exid = Expect(TK_IDENTIFIER); Expect(_SC(')'));
        {
            BEGIN_SCOPE();
            SQInteger ex_target = _fs->PushLocalVariable(exid, false);
            _fs->SetInstructionParam(trappos, 0, ex_target);
            c = parseStatement();
            _fs->SetInstructionParams(jmppos, 0, (_fs->GetCurrentPos() - jmppos), 0);
            END_SCOPE();
        }
        return new TryStatement(t, new Id(exid), c);
    }

    SQObject generateSurrogateFunctionName()
    {
        const SQChar * fileName = (sq_type(_sourcename) == OT_STRING) ? _stringval(_sourcename) : _SC("unknown");
        int lineNum = int(_lex._currentline);

        const SQChar * rightSlash = std::max(scstrrchr(fileName, _SC('/')), scstrrchr(fileName, _SC('\\')));

        SQChar buf[MAX_FUNCTION_NAME_LEN];
        scsprintf(buf, MAX_FUNCTION_NAME_LEN, _SC("(%s:%d)"), rightSlash ? (rightSlash + 1) : fileName, lineNum);
        return _fs->CreateString(buf);
    }

    DeclExpr *FunctionExp(SQInteger ftype,bool lambda = false)
    {
        Lex();
        SQObject functionName = (_token == TK_IDENTIFIER) ? Expect(TK_IDENTIFIER) : generateSurrogateFunctionName();
        Expect(_SC('('));

        DeclExpr *r = new DeclExpr(CreateFunction(functionName, lambda));
        _fs->AddInstruction(_OP_CLOSURE, _fs->PushTarget(), _fs->_functions.size() - 1, ftype == TK_FUNCTION?0:1);
        return r;
    }
    ClassDecl *ClassExp(Expr *key)
    {
        SQInteger base = -1;
        Expr *baseExpr = NULL;
        if(_token == TK_EXTENDS) {
            Lex(); 
            baseExpr = Expression(SQE_RVALUE);
            base = _fs->TopTarget();
        }
        Expect(_SC('{'));
        if(base != -1) _fs->PopTarget();
        ClassDecl *d = new ClassDecl(_ss(_vm)->_alloc_ctx, key, baseExpr);
        _fs->AddInstruction(_OP_NEWOBJ, _fs->PushTarget(), base, 0, NOT_CLASS);
        ParseTableOrClass(d, _SC(';'),_SC('}'));
        return d;
    }
    Expr *DeleteExpr()
    {
        SQExpState es;
        Lex();
        es = _es;
        _es.donot_get = true;
        Expr *arg = PrefixedExpr();
        if(_es.etype==EXPR) Error(_SC("can't delete an expression"));
        if(_es.etype==BASE) Error(_SC("can't delete 'base'"));
        if(_es.etype==OBJECT) {
            Emit2ArgsOP(_OP_DELETE);
        }
        else {
            Error(_SC("cannot delete an (outer) local"));
        }
        _es = es;
        return new UnExpr(TO_DELETE, arg);
    }
    Expr *PrefixIncDec(SQInteger token)
    {
        SQExpState  es;
        SQInteger diff = (token==TK_MINUSMINUS) ? -1 : 1;
        Lex();
        es = _es;
        _es.donot_get = true;
        Expr *arg = PrefixedExpr();
        if(_es.etype==EXPR) {
            Error(_SC("can't '++' or '--' an expression"));
        }
        else if (_es.etype == BASE) {
            Error(_SC("'base' cannot be modified"));
        }
        else if(_es.etype==OBJECT) {
            Emit2ArgsOP(_OP_INC, diff);
        }
        else if (_es.isBinding()) {
            Error(_SC("can't '++' or '--' a binding"));
        }
        else if(_es.etype==LOCAL) {
            SQInteger src = _fs->TopTarget();
            _fs->AddInstruction(_OP_INCL, src, src, 0, diff);

        }
        else if(_es.etype==OUTER) {
            SQInteger tmp = _fs->PushTarget();
            _fs->AddInstruction(_OP_GETOUTER, tmp, _es.epos);
            _fs->AddInstruction(_OP_INCL,     tmp, tmp, 0, diff);
            _fs->AddInstruction(_OP_SETOUTER, tmp, _es.epos, tmp);
        }
        _es = es;

        return new IncExpr(arg, diff, IF_PREFIX);
    }
    FunctionDecl *CreateFunction(SQObject &name,bool lambda = false, bool ctor = false)
    {
        SQFuncState *funcstate = _fs->PushChildState(_ss(_vm));
        FunctionDecl *f = ctor ? new ConstructorDecl(_ss(_vm)->_alloc_ctx, new Id(name)) : new FunctionDecl(_ss(_vm)->_alloc_ctx, new Id(name));
        funcstate->_name = name;
        SQObject paramname;
        SQObject thisName = _fs->CreateString(_SC("this"));
        f->addParameter(new Id(thisName));

        funcstate->AddParameter(thisName);
        funcstate->_sourcename = _sourcename;
        SQInteger defparams = 0;
        while(_token!=_SC(')')) {
            if(_token == TK_VARPARAMS) {
                if(defparams > 0) Error(_SC("function with default parameters cannot have variable number of parameters"));
                SQObject varargName = _fs->CreateString(_SC("vargv"));
                funcstate->AddParameter(varargName);
                f->addParameter(new Id(varargName));
                f->setVararg();
                funcstate->_varparams = true;
                Lex();
                if(_token != _SC(')')) Error(_SC("expected ')'"));
                break;
            }
            else {
                paramname = Expect(TK_IDENTIFIER);
                funcstate->AddParameter(paramname);
                Expr *defVal = NULL;
                if(_token == _SC('=')) {
                    Lex();
                    defVal = Expression(SQE_RVALUE);
                    funcstate->AddDefaultParam(_fs->TopTarget());
                    defparams++;
                }
                else {
                    if(defparams > 0) Error(_SC("expected '='"));
                }

                f->addParameter(new Id(paramname), defVal);

                if(_token == _SC(',')) Lex();
                else if(_token != _SC(')')) Error(_SC("expected ')' or ','"));
            }
        }
        Expect(_SC(')'));
        for(SQInteger n = 0; n < defparams; n++) {
            _fs->PopTarget();
        }

        SQFuncState *currchunk = _fs;
        _fs = funcstate;
        Statement *body = NULL;
        if(lambda) {
            _fs->AddLineInfos(_lex._prevtoken == _SC('\n') ? _lex._lasttokenline: _lex._currentline, _lineinfo, true);
            Expr *expr = Expression(SQE_REGULAR);
            body = new ReturnStatement(expr);
            _fs->AddInstruction(_OP_RETURN, 1, _fs->PopTarget());}
        else {
            if (_token != '{')
                Error(_SC("'{' expected"));
            body = parseStatement(false);
        }
        funcstate->AddLineInfos(_lex._prevtoken == _SC('\n')?_lex._lasttokenline:_lex._currentline, _lineinfo, true);
        funcstate->AddInstruction(_OP_RETURN, -1);

        if (!(funcstate->lang_features & LF_DISABLE_OPTIMIZER)) {
            SQOptimizer opt(*funcstate);
            opt.optimize();
        }
        f->setBody(body);

        funcstate->SetStackSize(0);

        SQFunctionProto *func = funcstate->BuildProto();
#ifdef _DEBUG_DUMP
        funcstate->Dump(func);
#endif
        _fs = currchunk;
        _fs->_functions.push_back(func);
        _fs->PopChildState();

        f->setSourceName(_sourcename);

        return f;
    }
    void ResolveBreaks(SQFuncState *funcstate, SQInteger ntoresolve)
    {
        while(ntoresolve > 0) {
            SQInteger pos = funcstate->_unresolvedbreaks.back();
            funcstate->_unresolvedbreaks.pop_back();
            //set the jmp instruction
            funcstate->SetInstructionParams(pos, 0, funcstate->GetCurrentPos() - pos, 0);
            ntoresolve--;
        }
    }
    void ResolveContinues(SQFuncState *funcstate, SQInteger ntoresolve, SQInteger targetpos)
    {
        while(ntoresolve > 0) {
            SQInteger pos = funcstate->_unresolvedcontinues.back();
            funcstate->_unresolvedcontinues.pop_back();
            //set the jmp instruction
            funcstate->SetInstructionParams(pos, 0, targetpos - pos, 0);
            ntoresolve--;
        }
    }

    bool CanBeDefaultDelegate(const SQObjectPtr &key)
    {
        if (sq_type(key) != OT_STRING)
            return false;

        // this can be optimized by keeping joined list/table of used keys
        SQTable *delegTbls[] = {
            _table(_fs->_sharedstate->_table_default_delegate),
            _table(_fs->_sharedstate->_array_default_delegate),
            _table(_fs->_sharedstate->_string_default_delegate),
            _table(_fs->_sharedstate->_number_default_delegate),
            _table(_fs->_sharedstate->_generator_default_delegate),
            _table(_fs->_sharedstate->_closure_default_delegate),
            _table(_fs->_sharedstate->_thread_default_delegate),
            _table(_fs->_sharedstate->_class_default_delegate),
            _table(_fs->_sharedstate->_instance_default_delegate),
            _table(_fs->_sharedstate->_weakref_default_delegate),
            _table(_fs->_sharedstate->_userdata_default_delegate)
        };
        SQObjectPtr tmp;
        for (SQInteger i=0; i<sizeof(delegTbls)/sizeof(delegTbls[0]); ++i) {
            if (delegTbls[i]->Get(key, tmp))
                return true;
        }
        return false;
    }

    SQObjectPtr sourcename() { return _sourcename; }

private:
    SQInteger _token;
    SQFuncState *_fs;
    SQObjectPtr _sourcename;
    SQLexer _lex;
    bool _lineinfo;
    bool _raiseerror;
    SQExpState   _es;
    SQScope _scope;
    SQExpressionContext _expression_context;
    SQChar _compilererror[MAX_COMPILER_ERROR_LEN];
    jmp_buf _errorjmp;
    SQVM *_vm;
    SQObjectPtrVec _scopedconsts;
    SQUnsignedInteger _num_initial_bindings;
    sqvector<sqvector<SQObject>*> _member_constant_keys_check;
};

static void ThrowError(void *ud, const SQChar *s) {
    SQCompiler *c = (SQCompiler *)ud;
    c->Error(s);
}


bool Compile(SQVM *vm,SQLEXREADFUNC rg, SQUserPointer up, const HSQOBJECT *bindings, const SQChar *sourcename, SQObjectPtr &out, bool raiseerror, bool lineinfo)
{
    SQCompiler p(vm, rg, up, bindings, sourcename, raiseerror, lineinfo);

    if (vm->_on_compile_file)
      vm->_on_compile_file(vm, sourcename);

    RootBlock *r = p.Compile(out);

    RenderVisitor v(std::cout);
    r->visit(v);

    SQFuncState::reset();

    SQFuncState funcstate(_ss(vm), NULL, ThrowError, &p);
    CodegenVisitor codegen(_ss(vm)->_alloc_ctx, funcstate, bindings, p, vm, p.sourcename(), out);

    r->visit(codegen);

    codegen.finish();

    return r != NULL;
}

CodegenVisitor::CodegenVisitor(SQAllocContext ctx, SQFuncState &root_fs, const HSQOBJECT *bindings, SQCompiler &compiler, SQVM *vm, SQObjectPtr sourceName, SQObjectPtr &o) : Visitor(), _fs(&root_fs), _funcState(&root_fs), _compiler(compiler), _scopedconsts(ctx), _vm(*vm), _o(o) {
    _fs->_name = SQString::Create(_ss(vm), _SC("__main__"));

    _fs->AddParameter(_fs->CreateString(_SC("this")));
    _fs->AddParameter(_fs->CreateString(_SC("vargv")));
    _fs->_varparams = true;
    _fs->_sourcename = sourceName;
    _fs->lang_features = compiler._lang_features;
    _stacksize = _fs->GetStackSize();

    _donot_get = false;

    if (bindings) {
        assert(sq_type(*bindings) == OT_TABLE || sq_type(*bindings) == OT_NULL);
        if (sq_type(*bindings) == OT_TABLE) {
            _scopedconsts.push_back(*bindings);
            _num_initial_bindings = 1;
        }
    }
}

void CodegenVisitor::finish() {
    _fs->SetStackSize(_stacksize);
    //_fs->AddLineInfos(_lex._currentline, _lineinfo, true);
    _fs->AddInstruction(_OP_RETURN, 0xFF);

    if (!(_fs->lang_features & LF_DISABLE_OPTIMIZER)) {
        SQOptimizer opt(*_fs);
        opt.optimize();
    }

    _fs->SetStackSize(0);

    _o = _fs->BuildProto();

    _fs->Dump(_funcproto(_o));
}

void CodegenVisitor::Emit2ArgsOP(SQOpcode op, SQInteger p3)
{
    SQInteger p2 = _fs->PopTarget(); //src in OP_GET
    SQInteger p1 = _fs->PopTarget(); //key in OP_GET
    _fs->AddInstruction(op, _fs->PushTarget(), p1, p2, p3);
}

void CodegenVisitor::EmitLoadConstInt(SQInteger value, SQInteger target)
{
    if (target < 0) {
        target = _fs->PushTarget();
    }
    if (value <= INT_MAX && value > INT_MIN) { //does it fit in 32 bits?
        _fs->AddInstruction(_OP_LOADINT, target, value);
    }
    else {
        _fs->AddInstruction(_OP_LOAD, target, _fs->GetNumericConstant(value));
    }
}

void CodegenVisitor::EmitLoadConstFloat(SQFloat value, SQInteger target)
{
    if (target < 0) {
        target = _fs->PushTarget();
    }
    if (sizeof(SQFloat) == sizeof(SQInt32)) {
        _fs->AddInstruction(_OP_LOADFLOAT, target, *((SQInt32 *)&value));
    }
    else {
        _fs->AddInstruction(_OP_LOAD, target, _fs->GetNumericConstant(value));
    }
}

void CodegenVisitor::ResolveBreaks(SQFuncState *funcstate, SQInteger ntoresolve)
{
    while (ntoresolve > 0) {
        SQInteger pos = funcstate->_unresolvedbreaks.back();
        funcstate->_unresolvedbreaks.pop_back();
        //set the jmp instruction
        funcstate->SetInstructionParams(pos, 0, funcstate->GetCurrentPos() - pos, 0);
        ntoresolve--;
    }
}
void CodegenVisitor::ResolveContinues(SQFuncState *funcstate, SQInteger ntoresolve, SQInteger targetpos)
{
    while (ntoresolve > 0) {
        SQInteger pos = funcstate->_unresolvedcontinues.back();
        funcstate->_unresolvedcontinues.pop_back();
        //set the jmp instruction
        funcstate->SetInstructionParams(pos, 0, targetpos - pos, 0);
        ntoresolve--;
    }
}

void CodegenVisitor::EmitDerefOp(SQOpcode op)
{
    SQInteger val = _fs->PopTarget();
    SQInteger key = _fs->PopTarget();
    SQInteger src = _fs->PopTarget();
    _fs->AddInstruction(op, _fs->PushTarget(), src, key, val);
}

void CodegenVisitor::visitBlock(Block *block) {
    BEGIN_SCOPE();

    const sqvector<Statement *> &statements = block->statements();

    for (int i = 0; i < statements.size(); ++i) {
        Statement *stmt = statements[i];
        stmt->visit(*this);
        _fs->SnoozeOpt();
    }

    END_SCOPE();
}

void CodegenVisitor::visitIfStatement(IfStatement *ifStmt) {
    BEGIN_SCOPE();

    ifStmt->condition()->visit(*this);

    _fs->AddInstruction(_OP_JZ, _fs->PopTarget());
    SQInteger jnepos = _fs->GetCurrentPos();
    
    ifStmt->thenBranch()->visit(*this);

    SQInteger endifblock = _fs->GetCurrentPos();

    if (ifStmt->elseBranch()) {
        _fs->AddInstruction(_OP_JMP);
        SQInteger jmppos = _fs->GetCurrentPos();
        ifStmt->elseBranch()->visit(*this);
        _fs->SetInstructionParam(jmppos, 1, _fs->GetCurrentPos() - jmppos);
    }

    _fs->SetInstructionParam(jnepos, 1, endifblock - jnepos + (ifStmt->elseBranch() ? 1 : 0));
    END_SCOPE();
}

void CodegenVisitor::visitWhileStatement(WhileStatement *whileLoop) {
    BEGIN_SCOPE();
    {
        SQInteger jmppos = _fs->GetCurrentPos();

        whileLoop->condition()->visit(*this);

        BEGIN_BREAKBLE_BLOCK();

        _fs->AddInstruction(_OP_JZ, _fs->PopTarget());

        SQInteger jzpos = _fs->GetCurrentPos();

        BEGIN_SCOPE();

        whileLoop->body()->visit(*this);

        END_SCOPE();

        _fs->AddInstruction(_OP_JMP, 0, jmppos - _fs->GetCurrentPos() - 1);
        _fs->SetInstructionParam(jzpos, 1, _fs->GetCurrentPos() - jzpos);

        END_BREAKBLE_BLOCK(jmppos);
    }
    END_SCOPE();
}

void CodegenVisitor::visitDoWhileStatement(DoWhileStatement *doWhileLoop) {
    BEGIN_SCOPE();
    {
        SQInteger jmptrg = _fs->GetCurrentPos();
        BEGIN_BREAKBLE_BLOCK();

        BEGIN_SCOPE();
        doWhileLoop->body()->visit(*this);
        END_SCOPE();

        SQInteger continuetrg = _fs->GetCurrentPos();
        doWhileLoop->condition()->visit(*this);

        _fs->AddInstruction(_OP_JZ, _fs->PopTarget(), 1);
        _fs->AddInstruction(_OP_JMP, 0, jmptrg - _fs->GetCurrentPos() - 1);
        END_BREAKBLE_BLOCK(continuetrg);

    }
    END_SCOPE();
}

void CodegenVisitor::visitForStatement(ForStatement *forLoop) {
    BEGIN_SCOPE();

    if (forLoop->initializer()) {
        Node *init = forLoop->initializer();
        init->visit(*this);
        if (init->isExpression()) {
            _fs->PopTarget();
        }
    }

    _fs->SnoozeOpt();
    SQInteger jmppos = _fs->GetCurrentPos();
    SQInteger jzpos = -1;

    if (forLoop->condition()) {
        forLoop->condition()->visit(*this);
        _fs->AddInstruction(_OP_JZ, _fs->PopTarget());
        jzpos = _fs->GetCurrentPos();
    }

    _fs->SnoozeOpt();

    SQInteger expstart = _fs->GetCurrentPos() + 1;

    if (forLoop->modifier()) {
        forLoop->modifier()->visit(*this);
        _fs->PopTarget();
    }

    _fs->SnoozeOpt();

    SQInteger expend = _fs->GetCurrentPos();
    SQInteger expsize = (expend - expstart) + 1;
    SQInstructionVec exp(_fs->_sharedstate->_alloc_ctx);

    if (expsize > 0) {
        for (SQInteger i = 0; i < expsize; i++)
            exp.push_back(_fs->GetInstruction(expstart + i));
        _fs->PopInstructions(expsize);
    }

    BEGIN_BREAKBLE_BLOCK();
    forLoop->body()->visit(*this);
    SQInteger continuetrg = _fs->GetCurrentPos();
    if (expsize > 0) {
        for (SQInteger i = 0; i < expsize; i++)
            _fs->AddInstruction(exp[i]);
    }

    _fs->AddInstruction(_OP_JMP, 0, jmppos - _fs->GetCurrentPos() - 1, 0);
    if (jzpos > 0) _fs->SetInstructionParam(jzpos, 1, _fs->GetCurrentPos() - jzpos);

    END_BREAKBLE_BLOCK(continuetrg);

    END_SCOPE();
}

void CodegenVisitor::visitForeachStatement(ForeachStatement *foreachLoop) {
    BEGIN_SCOPE();

    foreachLoop->container()->visit(*this);

    SQInteger container = _fs->TopTarget();
    SQInteger indexpos = _fs->PushLocalVariable(foreachLoop->idx()->id(), false);

    _fs->AddInstruction(_OP_LOADNULLS, indexpos, 1);

    SQInteger valuepos = _fs->PushLocalVariable(foreachLoop->val()->id(), false);
    _fs->AddInstruction(_OP_LOADNULLS, valuepos, 1);

    //push reference index
    SQInteger itrpos = _fs->PushLocalVariable(_fs->CreateString(_SC("@ITERATOR@")), false); //use invalid id to make it inaccessible
    _fs->AddInstruction(_OP_LOADNULLS, itrpos, 1);
    SQInteger jmppos = _fs->GetCurrentPos();
    _fs->AddInstruction(_OP_FOREACH, container, 0, indexpos);
    SQInteger foreachpos = _fs->GetCurrentPos();
    _fs->AddInstruction(_OP_POSTFOREACH, container, 0, indexpos);

    BEGIN_BREAKBLE_BLOCK();
    foreachLoop->body()->visit(*this);
    _fs->AddInstruction(_OP_JMP, 0, jmppos - _fs->GetCurrentPos() - 1);
    _fs->SetInstructionParam(foreachpos, 1, _fs->GetCurrentPos() - foreachpos);
    _fs->SetInstructionParam(foreachpos + 1, 1, _fs->GetCurrentPos() - foreachpos);
    END_BREAKBLE_BLOCK(foreachpos - 1);
    //restore the local variable stack(remove index,val and ref idx)
    _fs->PopTarget();
    END_SCOPE();
}

void CodegenVisitor::visitSwitchStatement(SwitchStatement *swtch) {
    BEGIN_SCOPE();

    swtch->expression()->visit(*this);

    SQInteger expr = _fs->TopTarget();
    SQInteger tonextcondjmp = -1;
    SQInteger skipcondjmp = -1;
    SQInteger __nbreaks__ = _fs->_unresolvedbreaks.size();

    _fs->_breaktargets.push_back(0);
    _fs->_blockstacksizes.push_back(_scope.stacksize);
    const sqvector<SwitchCase> &cases = swtch->cases();

    for (int i = 0; i < cases.size(); ++i) {
        if (i) {
            _fs->AddInstruction(_OP_JMP, 0, 0);
            skipcondjmp = _fs->GetCurrentPos();
            _fs->SetInstructionParam(tonextcondjmp, 1, _fs->GetCurrentPos() - tonextcondjmp);
        }

        const SwitchCase &c = cases[i];

        c.val->visit(*this);

        SQInteger trg = _fs->PopTarget();
        SQInteger eqtarget = trg;
        bool local = _fs->IsLocal(trg);
        if (local) {
            eqtarget = _fs->PushTarget(); //we need to allocate a extra reg
        }

        _fs->AddInstruction(_OP_EQ, eqtarget, trg, expr);
        _fs->AddInstruction(_OP_JZ, eqtarget, 0);
        if (local) {
            _fs->PopTarget();
        }

        //end condition
        if (skipcondjmp != -1) {
            _fs->SetInstructionParam(skipcondjmp, 1, (_fs->GetCurrentPos() - skipcondjmp));
        }
        tonextcondjmp = _fs->GetCurrentPos();

        BEGIN_SCOPE();
        c.stmt->visit(*this);
        END_SCOPE();
    }

    if (tonextcondjmp != -1)
        _fs->SetInstructionParam(tonextcondjmp, 1, _fs->GetCurrentPos() - tonextcondjmp);

    const SwitchCase &d = swtch->defaultCase();

    if (d.stmt) {
        BEGIN_SCOPE();
        d.stmt->visit(*this);
        END_SCOPE();
    }

    _fs->PopTarget();
    __nbreaks__ = _fs->_unresolvedbreaks.size() - __nbreaks__;
    if (__nbreaks__ > 0) ResolveBreaks(_fs, __nbreaks__);
    _fs->_breaktargets.pop_back();
    _fs->_blockstacksizes.pop_back();
    END_SCOPE();
}

void CodegenVisitor::visitTryStatement(TryStatement *tryStmt) {
    _fs->AddInstruction(_OP_PUSHTRAP, 0, 0);
    _fs->_traps++;

    if (_fs->_breaktargets.size()) _fs->_breaktargets.top()++;
    if (_fs->_continuetargets.size()) _fs->_continuetargets.top()++;

    SQInteger trappos = _fs->GetCurrentPos();
    {
        BEGIN_SCOPE();
        tryStmt->tryStatement()->visit(*this);
        END_SCOPE();
    }

    _fs->_traps--;
    _fs->AddInstruction(_OP_POPTRAP, 1, 0);
    if (_fs->_breaktargets.size()) _fs->_breaktargets.top()--;
    if (_fs->_continuetargets.size()) _fs->_continuetargets.top()--;
    _fs->AddInstruction(_OP_JMP, 0, 0);
    SQInteger jmppos = _fs->GetCurrentPos();
    _fs->SetInstructionParam(trappos, 1, (_fs->GetCurrentPos() - trappos));

    {
        BEGIN_SCOPE();
        SQInteger ex_target = _fs->PushLocalVariable(tryStmt->exceptionId()->id(), false);
        _fs->SetInstructionParam(trappos, 0, ex_target);
        tryStmt->catchStatement()->visit(*this);
        _fs->SetInstructionParams(jmppos, 0, (_fs->GetCurrentPos() - jmppos), 0);
        END_SCOPE();
    }
}

void CodegenVisitor::visitBreakStatement(BreakStatement *breakStmt) {
    if (_fs->_breaktargets.size() <= 0) _compiler.Error(_SC("'break' has to be in a loop block"));
    if (_fs->_breaktargets.top() > 0) {
        _fs->AddInstruction(_OP_POPTRAP, _fs->_breaktargets.top(), 0);
    }
    RESOLVE_OUTERS();
    _fs->AddInstruction(_OP_JMP, 0, -1234);
    _fs->_unresolvedbreaks.push_back(_fs->GetCurrentPos());
}

void CodegenVisitor::visitContinueStatement(ContinueStatement *continueStmt) {
    if (_fs->_continuetargets.size() <= 0) _compiler.Error(_SC("'continue' has to be in a loop block"));
    if (_fs->_continuetargets.top() > 0) {
        _fs->AddInstruction(_OP_POPTRAP, _fs->_continuetargets.top(), 0);
    }
    RESOLVE_OUTERS();
    _fs->AddInstruction(_OP_JMP, 0, -1234);
    _fs->_unresolvedcontinues.push_back(_fs->GetCurrentPos());
}

void CodegenVisitor::visitTerminateStatement(TerminateStatement *terminator) {
    if (terminator->argument()) {
        terminator->argument()->visit(*this);
    }
}

void CodegenVisitor::visitReturnStatement(ReturnStatement *retStmt) {
    SQInteger retexp = _fs->GetCurrentPos() + 1;
    visitTerminateStatement(retStmt);

    if (_fs->_traps > 0) {
        _fs->AddInstruction(_OP_POPTRAP, _fs->_traps, 0);
    }

    if (retStmt->argument()) {
        _fs->_returnexp = retexp;
        _fs->AddInstruction(_OP_RETURN, 1, _fs->PopTarget(), _fs->GetStackSize());
    }
    else {
        _fs->_returnexp = -1;
        _fs->AddInstruction(_OP_RETURN, 0xFF, 0, _fs->GetStackSize());
    }
}

void CodegenVisitor::visitYieldStatement(YieldStatement *yieldStmt) {
    SQInteger retexp = _fs->GetCurrentPos() + 1;
    _fs->_bgenerator = true;
    visitTerminateStatement(yieldStmt);

    if (yieldStmt->argument()) {
        _fs->_returnexp = retexp;
        _fs->AddInstruction(_OP_YIELD, 1, _fs->PopTarget(), _fs->GetStackSize());
    }
    else {
        _fs->_returnexp = -1;
        _fs->AddInstruction(_OP_YIELD, 0xFF, 0, _fs->GetStackSize());
    }
}

void CodegenVisitor::visitThrowStatement(ThrowStatement *throwStmt) {
    visitTerminateStatement(throwStmt);
    _fs->AddInstruction(_OP_THROW, _fs->PopTarget());
}

void CodegenVisitor::visitExprStatement(ExprStatement *stmt) {
    stmt->expression()->visit(*this);
    _fs->DiscardTarget();
}

void CodegenVisitor::visitTableDecl(TableDecl *tableDecl) {
    bool isKlass = tableDecl->op() == TO_CLASS;
    const auto members = tableDecl->members();

    if (!isKlass) {
        _fs->AddInstruction(_OP_NEWOBJ, _fs->PushTarget(), members.size(), 0, NOT_TABLE);
    }

    for (int i = 0; i < members.size(); ++i) {
        const TableMember &m = members[i];
        m.key->visit(*this);
        m.value->visit(*this);

        SQInteger val = _fs->PopTarget();
        SQInteger key = _fs->PopTarget();
        SQInteger table = _fs->TopTarget(); //<<BECAUSE OF THIS NO COMMON EMIT FUNC IS POSSIBLE

        if (isKlass) {
            _fs->AddInstruction(_OP_NEWSLOTA, m.isStatic ? NEW_SLOT_STATIC_FLAG : 0, table, key, val);
        }
        else {
            _fs->AddInstruction(_OP_NEWSLOT, 0xFF, table, key, val);
        }
    }
}

void CodegenVisitor::visitClassDecl(ClassDecl *klass) {

    if (klass->context() == DC_SLOT) {
        assert(klass->classKey());
        bool old_dng = _donot_get;
        _donot_get = true;
        klass->classKey()->visit(*this);
        _donot_get = old_dng;
    }

    Expr *baseExpr = klass->classBase();
    SQInteger baseIdx = -1;
    if (baseExpr) {
        baseExpr->visit(*this);
        baseIdx = _fs->PopTarget();
    }

    _fs->AddInstruction(_OP_NEWOBJ, _fs->PushTarget(), baseIdx, 0, NOT_CLASS);

    CodegenVisitor::visitTableDecl(klass);


    if (klass->context() == DC_SLOT) {
        EmitDerefOp(_OP_NEWSLOT);
        _fs->PopTarget();
    }
}

void CodegenVisitor::visitParamDecl(ParamDecl *param) {
    _funcState->AddParameter(param->name()->id());
    if (param->hasDefaultValue()) {
        param->defaultValue()->visit(*this);
    }
}

SQInteger _last_pop = -1;

void CodegenVisitor::visitVarDecl(VarDecl *var) {
    Id *name = var->name();

    if (var->initializer()) {
        var->initializer()->visit(*this);
        SQInteger src = _fs->PopTarget();
        SQInteger dest = _fs->PushTarget();
        if (dest != src) _fs->AddInstruction(_OP_MOVE, dest, src);
    }
    else {
        _fs->AddInstruction(_OP_LOADNULLS, _fs->PushTarget(), 1);
    }

    _last_pop = _fs->PopTarget();
    _fs->PushLocalVariable(name->id(), var->isAssignable());
}

void CodegenVisitor::visitDeclGroup(DeclGroup *group) {
    const auto declarations = group->declarations();

    for (int i = 0; i < declarations.size(); ++i) {
        Decl *d = declarations[i];
        d->visit(*this);
    }
}

void CodegenVisitor::visitDesctructionDecl(DesctructionDecl *destruct) {
    sqvector<SQInteger> targets(_ss(&_vm)->_alloc_ctx);

    const auto declarations = destruct->declarations();

    for (int i = 0; i < declarations.size(); ++i) {
        declarations[i]->visit(*this);
        assert(_last_pop != -1);
        targets.push_back(_last_pop);
        _last_pop = -1;
    }

    destruct->initiExpression()->visit(*this);

    SQInteger src = _fs->TopTarget();
    SQInteger key_pos = _fs->PushTarget();

    for (int i = 0; i < declarations.size(); ++i) {
        VarDecl *d = declarations[i];
        SQInteger flags = d->initializer() ? OP_GET_FLAG_NO_ERROR | OP_GET_FLAG_KEEP_VAL : 0;
        if (destruct->type() == DT_ARRAY) {
            EmitLoadConstInt(i, key_pos);
            _fs->AddInstruction(_OP_GET, targets[i], src, key_pos, flags);
        }
        else {
            _fs->AddInstruction(_OP_LOAD, key_pos, _fs->GetConstant(d->name()->id()));
            _fs->AddInstruction(_OP_GET, targets[i], src, key_pos, flags);
        }
    }

    _fs->PopTarget();
    _fs->PopTarget();
}

void CodegenVisitor::visitFunctionDecl(FunctionDecl *func) {
    SQFuncState *oldFuncState = _funcState;
    SQFuncState *funcstate = _fs->PushChildState(_ss(&_vm));
    funcstate->_name = func->name()->id();
    funcstate->_sourcename = func->sourceName();
    funcstate->_varparams = func->isVararg();

    const auto parameters = func->parameters();
    SQInteger defparams = 0;

    _funcState = funcstate;

    for (int i = 0; i < parameters.size(); ++i) {
        ParamDecl *param = parameters[i];
        if (param->hasDefaultValue()) ++defparams;
        param->visit(*this);
    }

    for (SQInteger n = 0; n < defparams; n++) {
        _fs->PopTarget();
    }

    SQFuncState *currchunk = _fs;
    _fs = _funcState;

    func->body()->visit(*this);
    // funcstate->AddLineInfos(_lex._prevtoken == _SC('\n')?_lex._lasttokenline:_lex._currentline, _lineinfo, true);
    SQInstruction &i = funcstate->GetInstruction(funcstate->GetCurrentPos());
    funcstate->AddInstruction(_OP_RETURN, -1);

    funcstate->SetStackSize(0);
    SQFunctionProto *funcProto = funcstate->BuildProto();

    _funcState = oldFuncState;
    _fs = currchunk;

    _fs->_functions.push_back(funcProto);
    funcstate->Dump(funcProto);
    _fs->PopChildState();

    _fs->AddInstruction(_OP_CLOSURE, _fs->PushTarget(), _fs->_functions.size() - 1, 0);

}

SQTable* CodegenVisitor::GetScopedConstsTable()
{
    assert(!_scopedconsts.empty());
    SQObjectPtr &consts = _scopedconsts.top();
    if (sq_type(consts) != OT_TABLE)
        consts = SQTable::Create(_ss(&_vm), 0);
    return _table(consts);
}

void CodegenVisitor::visitConstDecl(ConstDecl *decl) {
    SQObject id = decl->name()->id();
    SQObject value = decl->value();

    SQTable *enums = decl->isGlobal() ? _table(_ss(&_vm)->_consts) : GetScopedConstsTable();
    SQObjectPtr strongid = id;
    enums->NewSlot(strongid, SQObjectPtr(value));
    strongid.Null();
}

void CodegenVisitor::visitEnumDecl(EnumDecl *enums) {
    SQObject table = _fs->CreateTable();
    table._flags = SQOBJ_FLAG_IMMUTABLE;
    SQInteger nval = 0;

    const auto consts = enums->consts();

    for (int i = 0; i < consts.size(); ++i) {
        EnumConst &c = consts[i];
        _table(table)->NewSlot(SQObjectPtr(c.id), SQObjectPtr(c.val));
    }

    SQTable *enumsTable = enums->isGlobal() ? _table(_ss(&_vm)->_consts) : GetScopedConstsTable();

    SQObjectPtr strongid = enums->name();
    enumsTable->NewSlot(SQObjectPtr(strongid), SQObjectPtr(table));
    strongid.Null();
}

void CodegenVisitor::MoveIfCurrentTargetIsLocal() {
    SQInteger trg = _fs->TopTarget();
    if (_fs->IsLocal(trg)) {
        trg = _fs->PopTarget(); //pops the target and moves it
        _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), trg);
    }
}

bool isObject(Expr *expr) {
    if (expr->isAccessExpr()) return expr->asAccessExpr()->receiver()->op() != TO_BASE;
    if (expr->op() == TO_ID && expr->asId()->isField()) return true;
    return expr->op() == TO_ROOT;
}

bool isOuter(Expr *expr) {
    if (expr->op() != TO_ID) return false;
    return expr->asId()->isOuter();
}

void CodegenVisitor::visitCallExpr(CallExpr *call) {
    Expr *callee = call->callee();
    bool isNullCall = call->isNullable();

    bool old_dng = _donot_get;
    _donot_get = true;
    callee->visit(*this);
    _donot_get = old_dng;

    if (isObject(callee)) {
        if (!isNullCall) {
            SQInteger key = _fs->PopTarget();  /* location of the key */
            SQInteger table = _fs->PopTarget();  /* location of the object */
            SQInteger closure = _fs->PushTarget(); /* location for the closure */
            SQInteger ttarget = _fs->PushTarget(); /* location for 'this' pointer */
            _fs->AddInstruction(_OP_PREPCALL, closure, key, table, ttarget);
        }
        else {
            SQInteger self = _fs->GetUpTarget(1);  /* location of the object */
            SQInteger storedSelf = _fs->PushTarget();
            _fs->AddInstruction(_OP_MOVE, storedSelf, self);
            _fs->PopTarget();
            Emit2ArgsOP(_OP_GET, OP_GET_FLAG_NO_ERROR | OP_GET_FLAG_ALLOW_DEF_DELEGATE);
            SQInteger ttarget = _fs->PushTarget();
            _fs->AddInstruction(_OP_MOVE, ttarget, storedSelf);
        }
    }
    else if (isOuter(callee)) {
        _fs->AddInstruction(_OP_GETOUTER, _fs->PushTarget(), callee->asId()->outerPos());
        _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), 0);
    }
    else {
        _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), 0);
    }

    //switch (guessCalleeType(callee)) // TODO
    //{
    //case OBJECT:
    //    if (!isNullCall) {
    //        SQInteger key = _fs->PopTarget();  /* location of the key */
    //        SQInteger table = _fs->PopTarget();  /* location of the object */
    //        SQInteger closure = _fs->PushTarget(); /* location for the closure */
    //        SQInteger ttarget = _fs->PushTarget(); /* location for 'this' pointer */
    //        _fs->AddInstruction(_OP_PREPCALL, closure, key, table, ttarget);
    //    }
    //    else {
    //        SQInteger self = _fs->GetUpTarget(1);  /* location of the object */
    //        SQInteger storedSelf = _fs->PushTarget();
    //        _fs->AddInstruction(_OP_MOVE, storedSelf, self);
    //        _fs->PopTarget();
    //        Emit2ArgsOP(_OP_GET, OP_GET_FLAG_NO_ERROR | OP_GET_FLAG_ALLOW_DEF_DELEGATE);
    //        SQInteger ttarget = _fs->PushTarget();
    //        _fs->AddInstruction(_OP_MOVE, ttarget, storedSelf);
    //    }
    //    break;
    //case OUTER:
    //    _fs->AddInstruction(_OP_GETOUTER, _fs->PushTarget(), callee->asId()->outerPos());
    //case BASE:
    //default:
    //    _fs->AddInstruction(_OP_MOVE, _fs->PushTarget(), 0);
    //    break;
    //}

    const auto args = call->arguments();

    for (int i = 0; i < args.size(); ++i) {
        args[i]->visit(*this);
        MoveIfCurrentTargetIsLocal();
    }

    for (int i = 0; i < args.size(); ++i) {
        _fs->PopTarget();
    }

    SQInteger stackbase = _fs->PopTarget();
    SQInteger closure = _fs->PopTarget();
    SQInteger target = _fs->PushTarget();
    assert(target >= -1);
    assert(target < 255);
    _fs->AddInstruction(isNullCall ? _OP_NULLCALL : _OP_CALL, target, closure, stackbase, args.size() + 1);
}

void CodegenVisitor::visitBaseExpr(BaseExpr *base) {
    _fs->AddInstruction(_OP_GETBASE, _fs->PushTarget());
    base->setPos(_fs->TopTarget());
}

void CodegenVisitor::visitCalleeExpr(CalleeExpr *expr) {
    _fs->AddInstruction(_OP_LOADCALLEE, _fs->PushTarget());
}

void CodegenVisitor::visitRootExpr(RootExpr *expr) {
    _fs->AddInstruction(_OP_LOADROOT, _fs->PushTarget());
}

void CodegenVisitor::visitThisExpr(ThisExpr *expr) {

}

void CodegenVisitor::visitLiteralExpr(LiteralExpr *lit) {
    switch (lit->kind()) {
    case LK_STRING:
        _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(_fs->CreateString(lit->s())));
        break;
    case LK_FLOAT:EmitLoadConstFloat(lit->f(), -1); break;
    case LK_INT:  EmitLoadConstInt(lit->i(), -1); break;
    case LK_BOOL: _fs->AddInstruction(_OP_LOADBOOL, _fs->PushTarget(), lit->b()); break;
    case LK_NULL: _fs->AddInstruction(_OP_LOADNULLS, _fs->PushTarget(), 1); break;
    }
}

void CodegenVisitor::visitArrayExpr(ArrayExpr *expr) {
    const auto inits = expr->initialziers();

    _fs->AddInstruction(_OP_NEWOBJ, _fs->PushTarget(), inits.size(), 0, NOT_ARRAY);

    for (int i = 0; i < inits.size(); ++i) {
        inits[i]->visit(*this);
        SQInteger val = _fs->PopTarget();
        SQInteger array = _fs->TopTarget();
        _fs->AddInstruction(_OP_APPENDARRAY, array, val, AAT_STACK);
    }
}

void CodegenVisitor::emitUnaryOp(SQOpcode op, Expr *arg) {
    arg->visit(*this);
    SQInteger src = _fs->PopTarget();
    _fs->AddInstruction(op, _fs->PushTarget(), src);
}

void CodegenVisitor::emitDelete(Expr *argument) {
    bool old_dng = _donot_get;
    _donot_get = true;
    argument->visit(*this);
    _donot_get = old_dng;
    SQInteger table = _fs->PopTarget(); //src in OP_GET
    SQInteger key = _fs->PopTarget(); //key in OP_GET
    _fs->AddInstruction(_OP_DELETE, _fs->PushTarget(), key, table);
}

void CodegenVisitor::visitUnExpr(UnExpr *unary) {
    switch (unary->op())
    {
    case TO_NEG: emitUnaryOp(_OP_NEG, unary->argument()); break;
    case TO_NOT: emitUnaryOp(_OP_NOT, unary->argument()); break;
    case TO_BNOT:emitUnaryOp(_OP_BWNOT, unary->argument()); break;
    case TO_TYPEOF: emitUnaryOp(_OP_TYPEOF, unary->argument()); break;
    case TO_RESUME: emitUnaryOp(_OP_RESUME, unary->argument()); break;
    case TO_CLONE: emitUnaryOp(_OP_CLONE, unary->argument()); break;
    case TO_PAREN: unary->argument()->visit(*this); break;
    case TO_PDEC:
    case TO_PINC: // TODO
        break;
    case TO_DELETE: emitDelete(unary->argument());
        break;
    default:
        break;
    }
}

void CodegenVisitor::emitSimpleBin(SQOpcode op, Expr *lhs, Expr *rhs, SQInteger op3) {
    lhs->visit(*this);
    rhs->visit(*this);
    SQInteger op1 = _fs->PopTarget();
    SQInteger op2 = _fs->PopTarget();
    _fs->AddInstruction(op, _fs->PushTarget(), op1, op2, op3);
}

void CodegenVisitor::emitJpmArith(SQOpcode op, Expr *lhs, Expr *rhs) {
    lhs->visit(*this);
    rhs->visit(*this);

    SQInteger first_exp = _fs->PopTarget();
    SQInteger trg = _fs->PushTarget();
    _fs->AddInstruction(op, trg, 0, first_exp, 0);
    SQInteger jpos = _fs->GetCurrentPos();
    if (trg != first_exp) _fs->AddInstruction(_OP_MOVE, trg, first_exp);
    _fs->SnoozeOpt();
    SQInteger second_exp = _fs->PopTarget();
    if (trg != second_exp) _fs->AddInstruction(_OP_MOVE, trg, second_exp);
    _fs->SetInstructionParam(jpos, 1, (_fs->GetCurrentPos() - jpos));
    _fs->SnoozeOpt();
}

void CodegenVisitor::emitCompoundArith(SQOpcode op, SQInteger opcode, Expr *lvalue, Expr *rvalue) {

    bool old_dng = _donot_get;
    _donot_get = true;
    lvalue->visit(*this);
    _donot_get = old_dng;

    rvalue->visit(*this);

    if (lvalue->op() == TO_ID) {
        Id *id = lvalue->asId();
        if (id->isOuter()) {
            SQInteger val = _fs->TopTarget();
            SQInteger tmp = _fs->PushTarget();
            _fs->AddInstruction(_OP_GETOUTER, tmp, lvalue->asId()->outerPos());
            _fs->AddInstruction(op, tmp, val, tmp, 0);
            _fs->PopTarget();
            _fs->PopTarget();
            _fs->AddInstruction(_OP_SETOUTER, _fs->PushTarget(), lvalue->asId()->outerPos(), tmp);
        }
        else if (id->isLocal()) {
            SQInteger p2 = _fs->PopTarget(); //src in OP_GET
            SQInteger p1 = _fs->PopTarget(); //key in OP_GET
            _fs->PushTarget(p1);
            //EmitCompArithLocal(tok, p1, p1, p2);
            _fs->AddInstruction(op, p1, p2, p1, 0);
            _fs->SnoozeOpt();
        }
        else if (id->isField()) {
            SQInteger val = _fs->PopTarget();
            SQInteger key = _fs->PopTarget();
            SQInteger src = _fs->PopTarget();
            /* _OP_COMPARITH mixes dest obj and source val in the arg1 */
            _fs->AddInstruction(_OP_COMPARITH, _fs->PushTarget(), (src << 16) | val, key, opcode);
        }
        else {
            // TODO: err
        }
    }
    else if (lvalue->isAccessExpr()) {
        SQInteger val = _fs->PopTarget();
        SQInteger key = _fs->PopTarget();
        SQInteger src = _fs->PopTarget();
        /* _OP_COMPARITH mixes dest obj and source val in the arg1 */
        _fs->AddInstruction(_OP_COMPARITH, _fs->PushTarget(), (src << 16) | val, key, opcode);
    }
    else {
        // TODO: err
    }
}

void CodegenVisitor::emitNewSlot(Expr *lvalue, Expr *rvalue) {
    bool old_dng = _donot_get;
    _donot_get = true;
    lvalue->visit(*this);
    _donot_get = old_dng;
    rvalue->visit(*this);

    if (lvalue->isAccessExpr()) { // d.f || d["f"]
        SQInteger val = _fs->PopTarget();
        SQInteger key = _fs->PopTarget();
        SQInteger src = _fs->PopTarget();
        _fs->AddInstruction(_OP_NEWSLOT, _fs->PushTarget(), src, key, val);
    }
    else {
        _compiler.Error(_SC("can't 'create' a local slot"));
    }
}

void CodegenVisitor::emitFieldAssign(bool isLiteral) {
    SQInteger val = _fs->PopTarget();
    SQInteger key = _fs->PopTarget();
    SQInteger src = _fs->PopTarget();

    _fs->AddInstruction(isLiteral ? _OP_SET_LITERAL : _OP_SET, _fs->PushTarget(), src, key, val);
    SQ_STATIC_ASSERT(_OP_DATA_NOP == 0);
    if (isLiteral)
        _fs->AddInstruction(SQOpcode(0), 0, 0, 0, 0);//hint
}

void CodegenVisitor::emitAssign(Expr *lvalue, Expr * rvalue, bool inExpr) {
    bool old_dng = _donot_get;
    _donot_get = true;
    lvalue->visit(*this);
    _donot_get = old_dng;

    rvalue->visit(*this);

    if (inExpr) { // add extra checks

    }

    if (lvalue->op() == TO_ID) {
        Id *id = lvalue->asId();
        if (id->isOuter()) {
            SQInteger src = _fs->PopTarget();
            SQInteger dst = _fs->PushTarget();
            _fs->AddInstruction(_OP_SETOUTER, dst, id->outerPos(), src);
        }
        else if (id->isLocal()) {
            SQInteger src = _fs->PopTarget();
            SQInteger dst = _fs->TopTarget();
            _fs->AddInstruction(_OP_MOVE, dst, src);
        }
        else if (id->isField()) {
            emitFieldAssign(false);
        }
        else {
            // TODO: error
        }
    }
    else if (lvalue->isAccessExpr()) {
        emitFieldAssign(canBeLiteral(lvalue->asAccessExpr()));
    }
    else {
        // TODO: error
    }
}

bool CodegenVisitor::canBeLiteral(AccessExpr *expr) {
    if (!expr->isFieldAccessExpr()) return false;

    FieldAccessExpr *field = expr->asFieldAccessExpr();

    return field->canBeLiteral(CanBeDefaultDelegate(field->fieldName()));
}


bool CodegenVisitor::CanBeDefaultDelegate(const SQChar *key)
{
    // this can be optimized by keeping joined list/table of used keys
    SQTable *delegTbls[] = {
        _table(_fs->_sharedstate->_table_default_delegate),
        _table(_fs->_sharedstate->_array_default_delegate),
        _table(_fs->_sharedstate->_string_default_delegate),
        _table(_fs->_sharedstate->_number_default_delegate),
        _table(_fs->_sharedstate->_generator_default_delegate),
        _table(_fs->_sharedstate->_closure_default_delegate),
        _table(_fs->_sharedstate->_thread_default_delegate),
        _table(_fs->_sharedstate->_class_default_delegate),
        _table(_fs->_sharedstate->_instance_default_delegate),
        _table(_fs->_sharedstate->_weakref_default_delegate),
        _table(_fs->_sharedstate->_userdata_default_delegate)
    };
    SQObjectPtr tmp;
    for (SQInteger i = 0; i < sizeof(delegTbls) / sizeof(delegTbls[0]); ++i) {
        if (delegTbls[i]->GetStr(key, strlen(key), tmp))
            return true;
    }
    return false;
}

void CodegenVisitor::visitGetFieldExpr(GetFieldExpr *expr) {
    expr->receiver()->visit(*this);

    SQObject nameObj = _fs->CreateString(expr->fieldName());
    SQInteger constantI = _fs->GetConstant(nameObj);
    _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), constantI);

    SQInteger flags = expr->isNullable() ? OP_GET_FLAG_NO_ERROR : 0;

    bool defaultDelegate = CanBeDefaultDelegate(expr->fieldName());

    if (defaultDelegate) {
        flags |= OP_GET_FLAG_ALLOW_DEF_DELEGATE;
    }

    if (expr->receiver()->op() == TO_BASE) {
        Emit2ArgsOP(_OP_GET, flags);
    } else if (!_donot_get) {
        SQInteger src = _fs->PopTarget();
        SQInteger key = _fs->PopTarget();

        if (expr->canBeLiteral(defaultDelegate)) {
            _fs->AddInstruction(_OP_GET_LITERAL, _fs->PushTarget(), key, src, flags);
            SQ_STATIC_ASSERT(_OP_DATA_NOP == 0);
            _fs->AddInstruction(SQOpcode(0), 0, 0, 0, 0); //hint
        }
        else {
            _fs->AddInstruction(_OP_GET, _fs->PushTarget(), key, src, flags);
        }
    }
}

void CodegenVisitor::visitGetTableExpr(GetTableExpr *expr) {
    expr->receiver()->visit(*this);
    expr->key()->visit(*this);

    // TODO: wtf base?
    if (expr->receiver()->op() == TO_BASE) {
        Emit2ArgsOP(_OP_GET, expr->isNullable() ? OP_GET_FLAG_NO_ERROR : 0);
    } else if (!_donot_get) {
        SQInteger p2 = _fs->PopTarget(); //src in OP_GET
        SQInteger p1 = _fs->PopTarget(); //key in OP_GET
        _fs->AddInstruction(_OP_GET, _fs->PushTarget(), p1, p2, expr->isNullable() ? OP_GET_FLAG_NO_ERROR : 0);
    }
}

void CodegenVisitor::visitBinExpr(BinExpr *expr) {
    switch (expr->op()) {
    case TO_NEWSLOT: emitNewSlot(expr->lhs(), expr->rhs());  break;
    case TO_NULLC: emitJpmArith(_OP_NULLCOALESCE, expr->lhs(), expr->rhs()); break;
    case TO_OROR: emitJpmArith(_OP_OR, expr->lhs(), expr->rhs()); break;
    case TO_ANDAND: emitJpmArith(_OP_AND, expr->lhs(), expr->rhs()); break;
    case TO_INEXPR_ASSIGN: emitAssign(expr->lhs(), expr->rhs(), true); break;
    case TO_ASSIGN: emitAssign(expr->lhs(), expr->rhs(), false); break;
    case TO_PLUSEQ:  emitCompoundArith(_OP_ADD, '+', expr->lhs(), expr->rhs()); break;
    case TO_MINUSEQ: emitCompoundArith(_OP_SUB, '-', expr->lhs(), expr->rhs()); break;
    case TO_MULEQ:   emitCompoundArith(_OP_MUL, '*', expr->lhs(), expr->rhs()); break;
    case TO_DIVEQ:   emitCompoundArith(_OP_DIV, '/', expr->lhs(), expr->rhs()); break;
    case TO_MODEQ:   emitCompoundArith(_OP_MOD, '%', expr->lhs(), expr->rhs()); break;
    case TO_ADD: emitSimpleBin(_OP_ADD, expr->lhs(), expr->rhs()); break;
    case TO_SUB: emitSimpleBin(_OP_SUB, expr->lhs(), expr->rhs()); break;
    case TO_MUL: emitSimpleBin(_OP_MUL, expr->lhs(), expr->rhs()); break;
    case TO_DIV: emitSimpleBin(_OP_DIV, expr->lhs(), expr->rhs()); break;
    case TO_MOD: emitSimpleBin(_OP_MOD, expr->lhs(), expr->rhs()); break;
    case TO_OR:  emitSimpleBin(_OP_BITW, expr->lhs(), expr->rhs(), BW_OR); break;
    case TO_AND: emitSimpleBin(_OP_BITW, expr->lhs(), expr->rhs(), BW_AND); break;
    case TO_XOR: emitSimpleBin(_OP_BITW, expr->lhs(), expr->rhs(), BW_XOR); break;
    case TO_USHR:emitSimpleBin(_OP_BITW, expr->lhs(), expr->rhs(), BW_USHIFTR); break;
    case TO_SHR: emitSimpleBin(_OP_BITW, expr->lhs(), expr->rhs(), BW_SHIFTR); break;
    case TO_SHL: emitSimpleBin(_OP_BITW, expr->lhs(), expr->rhs(), BW_SHIFTL); break;
    case TO_EQ:  emitSimpleBin(_OP_EQ, expr->lhs(), expr->rhs()); break;
    case TO_NE:  emitSimpleBin(_OP_NE, expr->lhs(), expr->rhs()); break;
    case TO_GE:  emitSimpleBin(_OP_CMP, expr->lhs(), expr->rhs(), CMP_GE); break;
    case TO_GT:  emitSimpleBin(_OP_CMP, expr->lhs(), expr->rhs(), CMP_G); break;
    case TO_LE:  emitSimpleBin(_OP_CMP, expr->lhs(), expr->rhs(), CMP_LE); break;
    case TO_LT:  emitSimpleBin(_OP_CMP, expr->lhs(), expr->rhs(), CMP_L); break;
    case TO_3CMP: emitSimpleBin(_OP_CMP, expr->lhs(), expr->rhs(), CMP_3W); break;
    case TO_IN: emitSimpleBin(_OP_EXISTS, expr->lhs(), expr->rhs()); break;
    case TO_INSTANCEOF: emitSimpleBin(_OP_INSTANCEOF, expr->lhs(), expr->rhs()); break;
    default:
        break;
    }
}

void CodegenVisitor::visitTerExpr(TerExpr *expr) {
    assert(expr->op() == TO_TERNARY);

    expr->a()->visit(*this);
    _fs->AddInstruction(_OP_JZ, _fs->PopTarget());
    SQInteger jzpos = _fs->GetCurrentPos();

    SQInteger trg = _fs->PushTarget();
    expr->b()->visit(*this);
    SQInteger first_exp = _fs->PopTarget();
    if (trg != first_exp) _fs->AddInstruction(_OP_MOVE, trg, first_exp);
    SQInteger endfirstexp = _fs->GetCurrentPos();
    _fs->AddInstruction(_OP_JMP, 0, 0);
    SQInteger jmppos = _fs->GetCurrentPos();

    expr->c()->visit(*this);
    SQInteger second_exp = _fs->PopTarget();
    if (trg != second_exp) _fs->AddInstruction(_OP_MOVE, trg, second_exp);

    _fs->SetInstructionParam(jmppos, 1, _fs->GetCurrentPos() - jmppos);
    _fs->SetInstructionParam(jzpos, 1, endfirstexp - jzpos + 1);
    _fs->SnoozeOpt();
}

void CodegenVisitor::visitIncExpr(IncExpr *expr) {
    Expr *arg = expr->argument();
    bool old_dng = _donot_get;
    _donot_get = true;
    arg->visit(*this);
    _donot_get = old_dng;

    bool isPostfix = expr->form() == IF_POSTFIX;

    if (expr->isAccessExpr()) {
        Emit2ArgsOP(isPostfix ? _OP_PINC : _OP_INC, expr->diff());
    }
    else if (arg->op() == TO_ID) {
        Id *id = arg->asId();
        if (id->isOuter()) {
            SQInteger tmp1 = _fs->PushTarget();
            SQInteger tmp2 = isPostfix ? _fs->PushTarget() : tmp1;
            _fs->AddInstruction(_OP_GETOUTER, tmp2, id->outerPos());
            _fs->AddInstruction(_OP_PINCL, tmp1, tmp2, 0, expr->diff());
            _fs->AddInstruction(_OP_SETOUTER, tmp2, id->outerPos(), tmp2);
            if (isPostfix) {
                _fs->PopTarget();
            }
        }
        else if (id->isLocal()) {
            SQInteger src = isPostfix ? _fs->PopTarget() : _fs->TopTarget();
            SQInteger dst = isPostfix ? _fs->PushTarget() : src;
            _fs->AddInstruction(isPostfix ? _OP_PINCL : _OP_INCL, dst, src, 0, expr->diff());
        }
        else if (id->isField()) {
            Emit2ArgsOP(isPostfix ? _OP_PINC : _OP_INC, expr->diff());
        }
        else {
            // TODO: error
        }
    }
    else {
        // TODO: error
    }
}

bool CodegenVisitor::IsConstant(const SQObject &name, SQObject &e)
{
    if (IsLocalConstant(name, e))
        return true;
    if (IsGlobalConstant(name, e))
        return true;
    return false;
}

bool CodegenVisitor::IsLocalConstant(const SQObject &name, SQObject &e)
{
    SQObjectPtr val;
    for (SQInteger i = SQInteger(_scopedconsts.size()) - 1; i >= 0; --i) {
        SQObjectPtr &tbl = _scopedconsts[i];
        if (!sq_isnull(tbl) && _table(tbl)->Get(name, val)) {
            e = val;
            if (tbl._flags & SQOBJ_FLAG_IMMUTABLE)
                e._flags |= SQOBJ_FLAG_IMMUTABLE;
            return true;
        }
    }
    return false;
}

bool CodegenVisitor::IsGlobalConstant(const SQObject &name, SQObject &e)
{
    SQObjectPtr val;
    if (_table(_ss(&_vm)->_consts)->Get(name, val)) {
        e = val;
        return true;
    }
    return false;
}

void CodegenVisitor::visitId(Id *id) {
    SQInteger pos = -1;
    SQObject constant;
    SQObject idObj = id->id();
    bool assignable = false;

    if ((pos = _fs->GetLocalVariable(idObj, assignable)) != -1) {
        _fs->PushTarget(pos);
    }

    else if ((pos = _fs->GetOuterVariable(idObj, assignable)) != -1) {
        id->setOuterPos(pos);
        if (!_donot_get) {
            SQInteger stkPos = _fs->PushTarget();
            _fs->AddInstruction(_OP_GETOUTER, stkPos, pos);
        }
    }

    else if (IsConstant(idObj, constant)) {
        /* Handle named constant */
        SQObjectPtr constval = constant;
        //while (sq_type(constval) == OT_TABLE && (sq_objflags(constval) & SQOBJ_FLAG_IMMUTABLE) && _token == _SC('.')) {
        //    Expect('.');
        //    SQObject constid = Expect(TK_IDENTIFIER);
        //    if (!_table(constval)->Get(constid, constval)) {
        //        constval.Null();
        //        Error(_SC("invalid enum [no '%s' field in '%s']"), _stringval(constid), _stringval(id));
        //    }
        //}
        SQInteger stkPos = _fs->PushTarget();
        id->setConst();

        /* generate direct or literal function depending on size */
        SQObjectType ctype = sq_type(constval);
        switch (ctype) {
        case OT_INTEGER:
            EmitLoadConstInt(_integer(constval), stkPos);
            break;
        case OT_FLOAT:
            EmitLoadConstFloat(_float(constval), stkPos);
            break;
        case OT_BOOL:
            _fs->AddInstruction(_OP_LOADBOOL, stkPos, _integer(constval));
            break;
        default:
            _fs->AddInstruction(_OP_LOAD, stkPos, _fs->GetConstant(constval));
            break;
        }
    }
    else {
        /* Handle a non-local variable, aka a field. Push the 'this' pointer on
        * the virtual stack (always found in offset 0, so no instruction needs to
        * be generated), and push the key next. Generate an _OP_LOAD instruction
        * for the latter. If we are not using the variable as a dref expr, generate
        * the _OP_GET instruction.
        */
        if ((_fs->lang_features & LF_EXPLICIT_THIS)
            && !(_fs->lang_features & LF_TOOLS_COMPILE_CHECK))
            _compiler.Error(_SC("Unknown variable [%s]"), _stringval(idObj));

        _fs->PushTarget(0);
        _fs->AddInstruction(_OP_LOAD, _fs->PushTarget(), _fs->GetConstant(idObj));
        if (!_donot_get) {
            Emit2ArgsOP(_OP_GET);
        }
        id->setField();
        //_es.etype = OBJECT;
    }
}





#endif
