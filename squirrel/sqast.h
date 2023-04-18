#ifndef _SQAST_H_
#define _SQAST_H_

#include <assert.h>
#include "squirrel.h"
#include "squtils.h"
#include "sqobject.h"

#define TREE_OPS \
    DEF_TREE_OP(BLOCK), \
    DEF_TREE_OP(IF), \
    DEF_TREE_OP(WHILE), \
    DEF_TREE_OP(DOWHILE), \
    DEF_TREE_OP(FOR), \
    DEF_TREE_OP(FOREACH), \
    DEF_TREE_OP(SWITCH), \
    DEF_TREE_OP(RETURN), \
    DEF_TREE_OP(YIELD), \
    DEF_TREE_OP(THROW), \
    DEF_TREE_OP(TRY), \
    DEF_TREE_OP(BREAK), \
    DEF_TREE_OP(CONTINUE), \
    DEF_TREE_OP(EXPR_STMT), \
    DEF_TREE_OP(EMPTY), \
    DEF_TREE_OP(STATEMENT_MARK), \
    DEF_TREE_OP(ID), \
    DEF_TREE_OP(COMMA), \
    DEF_TREE_OP(NULLC), \
    DEF_TREE_OP(ASSIGN), \
    DEF_TREE_OP(OROR), \
    DEF_TREE_OP(ANDAND), \
    DEF_TREE_OP(OR), \
    DEF_TREE_OP(XOR), \
    DEF_TREE_OP(AND), \
    DEF_TREE_OP(NE), \
    DEF_TREE_OP(EQ), \
    DEF_TREE_OP(3CMP), \
    DEF_TREE_OP(GE), \
    DEF_TREE_OP(GT), \
    DEF_TREE_OP(LE), \
    DEF_TREE_OP(LT), \
    DEF_TREE_OP(IN), \
    DEF_TREE_OP(INSTANCEOF), \
    DEF_TREE_OP(USHR), \
    DEF_TREE_OP(SHR), \
    DEF_TREE_OP(SHL), \
    DEF_TREE_OP(MUL), \
    DEF_TREE_OP(DIV), \
    DEF_TREE_OP(MOD), \
    DEF_TREE_OP(ADD), \
    DEF_TREE_OP(SUB), \
    DEF_TREE_OP(NOT), \
    DEF_TREE_OP(BNOT), \
    DEF_TREE_OP(NEG), \
    DEF_TREE_OP(TYPEOF), \
    DEF_TREE_OP(RESUME), \
    DEF_TREE_OP(CLONE), \
    DEF_TREE_OP(DELETE), \
    DEF_TREE_OP(LITERAL), \
    DEF_TREE_OP(BASE), \
    DEF_TREE_OP(ROOT), \
    DEF_TREE_OP(THIS), \
    DEF_TREE_OP(CALLEE), \
    DEF_TREE_OP(PAREN), \
    DEF_TREE_OP(INC), \
    DEF_TREE_OP(DECL_EXPR), \
    DEF_TREE_OP(ARRAYEXPR), \
    DEF_TREE_OP(GETFIELD), \
    DEF_TREE_OP(SETFIELD), \
    DEF_TREE_OP(GETTABLE), \
    DEF_TREE_OP(SETTABLE), \
    DEF_TREE_OP(PINC), \
    DEF_TREE_OP(PDEC), \
    DEF_TREE_OP(CALL), \
    DEF_TREE_OP(TERNARY), \
    DEF_TREE_OP(INEXPR_ASSIGN), \
    DEF_TREE_OP(NEWSLOT), \
    DEF_TREE_OP(PLUSEQ), \
    DEF_TREE_OP(MINUSEQ), \
    DEF_TREE_OP(MULEQ), \
    DEF_TREE_OP(DIVEQ), \
    DEF_TREE_OP(MODEQ), \
    DEF_TREE_OP(EXPR_MARK), \
    DEF_TREE_OP(VAR), \
    DEF_TREE_OP(PARAM), \
    DEF_TREE_OP(CONST), \
    DEF_TREE_OP(DECL_GROUP), \
    DEF_TREE_OP(DESTRUCT), \
    DEF_TREE_OP(FUNCTION), \
    DEF_TREE_OP(CONSTRUCTOR), \
    DEF_TREE_OP(CLASS), \
    DEF_TREE_OP(ENUM), \
    DEF_TREE_OP(TABLE), \
    DEF_TREE_OP(ARRAY)

enum TreeOp {
#define DEF_TREE_OP(arg) TO_##arg
    TREE_OPS
#undef DEF_TREE_OP

};

class Visitor;

class Id;
class GetFieldExpr;
class GetTableExpr;

class Node {
protected:
    Node(enum TreeOp op): _op(op), _linepos(-1) {}
public:
    virtual ~Node() {}

    enum TreeOp op() const { return _op; }

    virtual void visit(Visitor &visitor) { visitChildren(visitor); }
    virtual void visitChildren(Visitor &visitor) = 0;

    bool isDeclaration() const { return _op > TO_EXPR_MARK; }
    bool isStatement() const { return _op < TO_STATEMENT_MARK; }
    bool isExpression() const { return TO_STATEMENT_MARK < _op && _op < TO_EXPR_MARK; }

    Id *asId() { assert(_op == TO_ID); return (Id*)this; }
    GetFieldExpr *asGetField() { assert(_op == TO_GETFIELD); return (GetFieldExpr*)this; }
    GetTableExpr *asGetTable() { assert(_op == TO_GETTABLE); return (GetTableExpr*)this; }

    SQInteger linePos() const { return _linepos; }
    void setLinePos(SQInteger pos) { _linepos = pos; }

private:

    SQInteger _linepos;

    enum TreeOp _op;
};

class AccessExpr;

class Expr : public Node {
protected:
    Expr(enum TreeOp op) : Node(op) {}

public:
    bool isAccessExpr() const { return TO_GETFIELD <= op() && op() <= TO_SETTABLE; }
    AccessExpr *asAccessExpr() const { assert(isAccessExpr()); return (AccessExpr*)this; }
    void visit(Visitor &visitor) override;
};

enum IdType : SQInteger {
    ID_LOCAL = -1,
    ID_CONST = -2,
    ID_FIELD = -3
};

class Id : public Expr {
public:
    Id(const SQChar *id) : Expr(TO_ID), _id(id), _outpos(ID_LOCAL) { 
        _id = strdup(id); // TODO
    }

    void visit(Visitor &visitor);
    void visitChildren(Visitor &visitor);

    const SQChar *id() { return _id; }

    void setOuterPos(SQInteger pos) { _outpos = pos; }
    
    bool isOuter() const { return _outpos >= 0; }
    bool isField() const { return _outpos == ID_FIELD; }
    void setField() { _outpos = ID_FIELD; }
    bool isLocal() const { return _outpos == ID_LOCAL; }
    bool isConst() const { return _outpos == ID_CONST; }
    void setConst() { _outpos = ID_CONST; }

    SQInteger outerPos() const { return _outpos; }


//private:
    const SQChar *_id;
    SQInteger _outpos;
};

class UnExpr : public Expr {
public:
    UnExpr(enum TreeOp op, Expr *arg): Expr(op), _arg(arg) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *argument() const { return _arg; }

//private:
    Expr *_arg;
};

class BinExpr : public Expr {
public:
    BinExpr(enum TreeOp op, Expr *lhs, Expr *rhs) : Expr(op), _lhs(lhs), _rhs(rhs) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *lhs() const { return _lhs; }
    Expr *rhs() const { return _rhs; }

    Expr *_lhs;
    Expr *_rhs;
};

class TerExpr : public Expr {
public:
    TerExpr(Expr *a, Expr *b, Expr *c) : Expr(TO_TERNARY), _a(a), _b(b), _c(c) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *a() const { return _a; }
    Expr *b() const { return _b; }
    Expr *c() const { return _c; }

private:

    Expr *_a;
    Expr *_b;
    Expr *_c;
};

class FieldAccessExpr;

class AccessExpr : public Expr {
protected:
    AccessExpr(enum TreeOp op, Expr *receiver, bool nullable) : Expr(op), _receiver(receiver), _nullable(nullable) {}
public:

    void visit(Visitor &visitor) = 0;
    void visitChildren(Visitor &visitor) = 0;

    bool isFieldAccessExpr() const { return op() == TO_GETFIELD || op() == TO_SETFIELD; }
    FieldAccessExpr *asFieldAccessExpr() const { assert(isFieldAccessExpr()); return (FieldAccessExpr *)this; }

    bool isNullable() const { return _nullable; }
    Expr *receiver() const { return _receiver; }

private:
    Expr *_receiver;
    bool _nullable;
};

class FieldAccessExpr : public AccessExpr {
protected:
    FieldAccessExpr(enum TreeOp op, Expr *receiver, const SQChar *field, bool nullable) : AccessExpr(op, receiver, nullable), _fieldName(field) {}
    //bool canBeDefaultDelegate() const;

public:

    bool canBeLiteral(bool defaultDelegate) const { return receiver()->op() != TO_BASE && !isNullable() && !defaultDelegate; }
    const SQChar *fieldName() const { return _fieldName; }

private:
    const SQChar *_fieldName;

};

class GetFieldExpr : public FieldAccessExpr {
public:
    GetFieldExpr(Expr *receiver, const SQChar *field, bool nullable): FieldAccessExpr(TO_GETFIELD, receiver, field, nullable) { }

    void visit(Visitor &visitor);
    void visitChildren(Visitor &visitor);
};


class SetFieldExpr : public FieldAccessExpr {
public:
    SetFieldExpr(Expr *receiver, const SQChar *field, Expr *value, bool nullable): FieldAccessExpr(TO_SETFIELD, receiver, field, nullable), _value(value) { }

    void visit(Visitor &visitor);
    void visitChildren(Visitor &visitor);

    Expr *value() const { return _value; }

private:
    Expr *_value;
};


class TableAccessExpr : public AccessExpr {
protected:
    TableAccessExpr(enum TreeOp op, Expr *receiver, Expr *key, bool nullable) : AccessExpr(op, receiver, nullable), _key(key) {}
public:

    Expr *key() const { return _key; }
private:
    Expr *_key;
};

class GetTableExpr : public TableAccessExpr {
public:
    GetTableExpr(Expr *receiver, Expr *key, bool nullable): TableAccessExpr(TO_GETTABLE, receiver, key, nullable) { }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
};

class SetTableExpr : public TableAccessExpr {
public:
    SetTableExpr(Expr *receiver, Expr *key, Expr *val, bool nullable): TableAccessExpr(TO_SETTABLE, receiver, key, nullable), _val(val) { }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *value() const { return _val; }
private:
    Expr *_val;
};

class BaseExpr : public Expr {
public:
    BaseExpr() : Expr(TO_BASE) {}
    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    void setPos(SQInteger pos) { _pos = pos; }
    SQInteger getPos() const { return _pos; }

private:
    SQInteger _pos;
};

class RootExpr : public Expr {
public:
    RootExpr() : Expr(TO_ROOT) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
};

class ThisExpr : public Expr {
public:
    ThisExpr() : Expr(TO_THIS) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
};

class CalleeExpr : public Expr {
public:
    CalleeExpr() : Expr(TO_CALLEE) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
};

enum LiteralKind {
    LK_STRING,
    LK_INT,
    LK_FLOAT,
    LK_BOOL,
    LK_NULL
};

class LiteralExpr : public Expr {
public:

    LiteralExpr() : Expr(TO_LITERAL), _kind(LK_NULL) {}
    LiteralExpr(const SQChar *s) : Expr(TO_LITERAL), _kind(LK_STRING) { 
        size_t l = strlen(s);
        SQChar *dst = new SQChar[l + 1];
        // TODO
        memcpy(dst, s, (l + 1) * sizeof (SQChar));
        _v.s = dst;
    }
    LiteralExpr(SQFloat f) : Expr(TO_LITERAL), _kind(LK_FLOAT) { _v.f = f; }
    LiteralExpr(SQInteger i) : Expr(TO_LITERAL), _kind(LK_INT) { _v.i = i; }
    LiteralExpr(bool i) : Expr(TO_LITERAL), _kind(LK_BOOL) { _v.i = i; }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    enum LiteralKind kind() const { return _kind;  }

    SQFloat f() const { assert(_kind == LK_FLOAT); return _v.f; }
    SQInteger i() const { assert(_kind == LK_INT); return _v.i; }
    bool b() const { assert(_kind == LK_BOOL); return _v.b; }
    const SQChar *s() const { assert(_kind == LK_STRING); return _v.s; }
    void *null() const { assert(_kind == LK_NULL); return nullptr; }

private:
    enum LiteralKind _kind;
    union {
        const SQChar *s;
        SQInteger i;
        SQFloat f;
        bool b;
    } _v;

};

enum IncForm {
    IF_PREFIX,
    IF_POSTFIX
};

class IncExpr : public Expr {
public:
    IncExpr(Expr *arg, SQInteger diff, enum IncForm form) : Expr(TO_INC), _arg(arg), _diff(diff), _form(form) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    enum IncForm form() const { return _form; }
    SQInteger diff() const { return _diff; }
    Expr *argument() const { return _arg; }

//private:
    enum IncForm _form;
    SQInteger _diff;
    Expr *_arg;
};

class Decl;

class DeclExpr : public Expr {
public:
    DeclExpr(Decl *decl) : Expr(TO_DECL_EXPR), _decl(decl) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
//private:
    Decl *_decl;
};

class CallExpr : public Expr {
public:
    CallExpr(SQAllocContext ctx, Expr *callee, bool nullable) : Expr(TO_CALL), _callee(callee), _args(ctx), _nullable(nullable) {}

    void addArgument(Expr *arg) { _args.push_back(arg); }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    bool isNullable() const { return _nullable; }
    Expr *callee() const { return _callee; }
    const sqvector<Expr *> &arguments() const { return _args; }
    sqvector<Expr *> &arguments() { return _args; }

//private:
    Expr *_callee;
    sqvector<Expr *> _args;
    bool _nullable;
};

class ArrayExpr : public Expr {
public:
    ArrayExpr(SQAllocContext ctx) : Expr(TO_ARRAYEXPR), _inits(ctx) {}

    void addValue(Expr *v) { _inits.push_back(v); }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    const sqvector<Expr *> &initialziers() const { return _inits; }

//private:
    sqvector<Expr *> _inits;
};

class CommaExpr : public Expr {
public:
    CommaExpr(SQAllocContext ctx) : Expr(TO_COMMA), _exprs(ctx) {}

    void addExpression(Expr *expr) { _exprs.push_back(expr); }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

//private:
    sqvector<Expr *> _exprs;
};

class Statement : public Node {
protected:
    Statement(enum TreeOp op) : Node(op) {}
public:
    void visit(Visitor &visitor) override;
};

enum DeclarationContext {
    DC_LOCAL,
    DC_SLOT,
    DC_EXPR
};

class Decl : public Statement {
protected:
    Decl(enum TreeOp op) : Statement(op) {}
public:
    void visit(Visitor &visitor) override;

    void setContext(enum DeclarationContext ctx) { _context = ctx; }
    enum DeclarationContext context() const { return _context; }

private:
    enum DeclarationContext _context;
};

class ValueDecl : public Decl {
protected:
    ValueDecl(enum TreeOp op, Id *name, Expr *expr) : Decl(op), _name(name), _expr(expr) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
//private:

    Expr *expression() const { return _expr; }

public:

    Id *name() const { return _name; }

    Id *_name;
    Expr *_expr;
};

class ParamDecl : public ValueDecl {
public:
    ParamDecl(Id *name, Expr *defaltVal) : ValueDecl(TO_PARAM, name, defaltVal) {}

    void visit(Visitor &visitor) override;

    bool hasDefaultValue() const { return expression() != NULL; }
    Expr *defaultValue() const { return expression(); }
};

class VarDecl : public ValueDecl {
public:
    VarDecl(Id *name, Expr *init, bool assignable) : ValueDecl(TO_VAR, name, init), _assignable(assignable) {}

    Expr *initializer() const { return expression(); }

    bool isAssignable() const { return _assignable; }

    void visit(Visitor &visitor) override;
private:
    bool _assignable;
};

struct TableMember {
    Expr *key;
    Node *value;
    bool isStatic;
};

class TableDecl : public Decl {
public:
    TableDecl(SQAllocContext ctx) : Decl(TO_TABLE), _members(ctx) {}

    void addMember(Expr *key, Node *value, bool isStatic = false) { _members.push_back({ key, value, isStatic }); }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    sqvector<TableMember> &members() { return _members; }
    const sqvector<TableMember> &members() const { return _members; }

protected:
    TableDecl(SQAllocContext ctx, enum TreeOp op) : Decl(op), _members(ctx) {}
//private:
public:
    sqvector<TableMember> _members;
};

class ClassDecl : public TableDecl {
public:
    ClassDecl(SQAllocContext ctx, Expr *key, Expr *base) : TableDecl(ctx, TO_CLASS), _key(key), _base(base) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *classBase() const { return _base; }
    Expr* classKey() const { return _key; }

//private:
    Expr *_key;
    Expr *_base;

};

class FunctionDecl : public Decl {
protected:
    FunctionDecl(enum TreeOp op, SQAllocContext ctx, Id *name) : Decl(op), _parameters(ctx), _name(name), _vararg(false) {}
public:
    FunctionDecl(SQAllocContext ctx, Id *name) : Decl(TO_FUNCTION), _parameters(ctx), _name(name), _vararg(false) {}

    void addParameter(Id *name, Expr *defaultVal = NULL) { _parameters.push_back(new ParamDecl(name, defaultVal)); }
    
    sqvector<ParamDecl *> &parameters() { return _parameters; }
    const sqvector<ParamDecl *> &parameters() const { return _parameters; }

    void setVararg() { _vararg = true; }
    void setBody(Statement *body) { _body = body; }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Id *name() const { return _name; }
    bool isVararg() const { return _vararg; }
    Statement *body() const { return _body; }

    void setSourceName(const SQChar *sn) { _sourcename = sn; }
    const SQChar *sourceName() const { return _sourcename; }


//private:
    Id *_name;
    sqvector<ParamDecl *> _parameters;
    Statement * _body;
    bool _vararg;
    
    const SQChar *_sourcename;

};

class ConstructorDecl : public FunctionDecl {
public:
    ConstructorDecl(SQAllocContext ctx, Id *name) : FunctionDecl(TO_CONSTRUCTOR, ctx, name) {}

    void visit(Visitor &visitor) override;
};

struct EnumConst {
    Id *id;
    LiteralExpr *val;
};

class EnumDecl : public Decl {
public:
    EnumDecl(SQAllocContext ctx, Id *id, bool global) : Decl(TO_ENUM), _id(id), _consts(ctx), _global(global) {}

    void addConst(Id *id, LiteralExpr *val) { _consts.push_back({ id, val }); }

    sqvector<EnumConst> &consts() { return _consts; }
    const sqvector<EnumConst> &consts() const { return _consts; }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Id *name() const { return _id; }
    bool isGlobal() const { return _global; }

//private:
    sqvector<EnumConst> _consts;
    Id *_id;
    bool _global;
};

class ConstDecl : public Decl {
public:
    ConstDecl(Id *id, LiteralExpr *value, bool global) : Decl(TO_CONST), _id(id), _value(value), _global(global) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Id *name() const { return _id; }
    LiteralExpr *value() const { return _value; }
    bool isGlobal() const { return _global; }

//private:
    Id *_id;
    LiteralExpr *_value;
    bool _global;
};

class DeclGroup : public Decl {
protected:
    DeclGroup(SQAllocContext ctx, enum TreeOp op) : Decl(op), _decls(ctx) {}
public:
    DeclGroup(SQAllocContext ctx) : Decl(TO_DECL_GROUP), _decls(ctx) {}

    void addDeclaration(VarDecl *d) { _decls.push_back(d); }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    sqvector<VarDecl *> &declarations() { return _decls; }
    const sqvector<VarDecl *> &declarations() const { return _decls; }

private:
    sqvector<VarDecl *> _decls;
};

enum DestructuringType {
    DT_TABLE,
    DT_ARRAY
};

class DesctructionDecl : public DeclGroup {
public:
    DesctructionDecl(SQAllocContext ctx, enum DestructuringType dt) : DeclGroup(ctx, TO_DESTRUCT), _dt_type(dt) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    void setExpression(Expr *expr) { _expr = expr; }
    Expr *initiExpression() const { return _expr; }

    enum DestructuringType type() const { return _dt_type; }

//private:
    Expr *_expr;
    enum DestructuringType _dt_type;
};

class Block : public Statement {
public:
    Block(SQAllocContext ctx, bool is_root = false) : Statement(TO_BLOCK), _statements(ctx), _is_root(is_root), _endLine(-1) {}

    void addStatement(Statement *stmt) { _statements.push_back(stmt); }

    sqvector<Statement *> &statements() { return _statements; }
    const sqvector<Statement *> &statements() const { return _statements; }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    bool isRoot() const { return _is_root; }

    void setStartLine(SQInteger l) { setLinePos(l); }
    void setEndLine(SQInteger l) { _endLine = l; }

    SQInteger startLine() const { return linePos(); }
    SQInteger endLine() const { return _endLine; }
private:
    SQInteger _endLine;
    bool _is_root;
    sqvector<Statement *> _statements;
};

class RootBlock : public Block {
public:
    RootBlock(SQAllocContext ctx) : Block(ctx, true) {}


};

class IfStatement : public Statement {
public:
    IfStatement(Expr *cond, Statement *thenB, Statement *elseB) : Statement(TO_IF), _cond(cond), _thenB(thenB), _elseB(elseB) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *condition() const { return _cond; }
    Statement *thenBranch() const { return _thenB; }
    Statement *elseBranch() const { return _elseB; }

//private:
    Expr *_cond;
    Statement *_thenB;
    Statement *_elseB;
};

class LoopStatement : public Statement {
protected:
    LoopStatement(enum TreeOp op, Statement *body) : Statement(op), _body(body) {}
public:
    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Statement *body() const { return _body; }

//private:
public:
    Statement *_body;
};

class WhileStatement : public LoopStatement {
public:
    WhileStatement(Expr *cond, Statement *body) : LoopStatement(TO_WHILE, body), _cond(cond) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *condition() const { return _cond;  }

//private:
    Expr *_cond;
};

class DoWhileStatement : public LoopStatement {
public:
    DoWhileStatement(Statement *body, Expr *cond) : LoopStatement(TO_DOWHILE, body), _cond(cond) {}
    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *condition() const { return _cond; }

//private:
    Expr *_cond;
};

class ForStatement : public LoopStatement {
public:
    ForStatement(Node *init, Expr *cond, Expr *mod, Statement *body) : LoopStatement(TO_FOR, body), _init(init), _cond(cond), _mod(mod) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Node *initializer() const { return _init; }
    Expr *condition() const { return _cond; }
    Expr *modifier() const { return _mod; }


//private:
    Node *_init;
    Expr *_cond;
    Expr *_mod;
};

class ForeachStatement : public LoopStatement {
public:
    ForeachStatement(Id *idx, Id *val, Expr *container, Statement *body) : LoopStatement(TO_FOREACH, body), _idx(idx), _val(val), _container(container) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *container() const { return _container; }
    Id *idx() const { return _idx; }
    Id *val() const { return _val; }

//private:
    Id *_idx;
    Id *_val;
    Expr *_container;
};

struct SwitchCase {
    Expr *val;
    Statement *stmt;
};

class SwitchStatement : public Statement {
public:
    SwitchStatement(SQAllocContext ctx, Expr *expr) : Statement(TO_SWITCH), _expr(expr), _cases(ctx) {}

    void addCases(Expr *val, Statement *stmt) { _cases.push_back({ val, stmt }); }

    void addDefault(Statement *stmt) {
        assert(_defaultCase.stmt == NULL);
        _defaultCase.stmt = stmt;
    }

    sqvector<SwitchCase> &cases() { return _cases; }
    const sqvector<SwitchCase> &cases() const { return _cases; }

    Expr *expression() const { return _expr; }
    const SwitchCase &defaultCase() const { return _defaultCase; }

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

//private:
    Expr *_expr;
    sqvector<SwitchCase> _cases;
    SwitchCase _defaultCase;
};

class TryStatement : public Statement {
public:
    TryStatement(Statement *t, Id *exc, Statement *c) : Statement(TO_TRY), _tryStmt(t), _exception(exc), _catchStmt(c) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Statement *tryStatement() const { return _tryStmt; }
    Id *exceptionId() const { return _exception; }
    Statement *catchStatement() const { return _catchStmt; }

//private:
    Statement *_tryStmt;
    Id *_exception;
    Statement *_catchStmt;
};

class TerminateStatement : public Statement {
protected:
    TerminateStatement(enum TreeOp op, Expr *arg) : Statement(op), _arg(arg) {}
public:
    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *argument() const { return _arg; }

//private:
public:
    Expr *_arg;
};

class ReturnStatement : public TerminateStatement {
public:
    ReturnStatement(Expr *arg) : TerminateStatement(TO_RETURN, arg), _isLambda(false) {}

    void visit(Visitor &visitor) override;

    void setIsLambda() { _isLambda = true; }
    bool isLambdaReturn() const { return _isLambda; }

private:
    bool _isLambda;
};

class YieldStatement : public TerminateStatement {
public:
    YieldStatement(Expr *arg) : TerminateStatement(TO_YIELD, arg) {}

    void visit(Visitor &visitor) override;
};

class ThrowStatement : public TerminateStatement {
public:
    ThrowStatement(Expr *arg) : TerminateStatement(TO_THROW, arg) { assert(arg); }

    void visit(Visitor &visitor) override;
};

class JumpStatement : public Statement {
protected:
    JumpStatement(enum TreeOp op) : Statement(op) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
};

class BreakStatement : public JumpStatement {
public:
    BreakStatement(Statement *breakTarget) : JumpStatement(TO_BREAK), _target(breakTarget) {}
    void visit(Visitor &visitor) override;

private:
    Statement *_target;
};

class ContinueStatement : public JumpStatement {
public:
    ContinueStatement(LoopStatement *target) : JumpStatement(TO_CONTINUE), _target(target) {}

    void visit(Visitor &visitor) override;
private:
    LoopStatement *_target;
};

class ExprStatement : public Statement {
public:
    ExprStatement(Expr *expr) : Statement(TO_EXPR_STMT), _expr(expr) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;

    Expr *expression() const { return _expr; }

//private:
    Expr *_expr;
};

class EmptyStatement : public Statement {
public:
    EmptyStatement() : Statement(TO_EMPTY) {}

    void visit(Visitor &visitor) override;
    void visitChildren(Visitor &visitor) override;
};

class Visitor {
protected:
    Visitor() {}
public:
    virtual ~Visitor() {}

    virtual void visitNode(Node *node) { node->visitChildren(*this); }

    virtual void visitExpr(Expr *expr) { visitNode(expr); }
    virtual void visitUnExpr(UnExpr *expr) { visitExpr(expr); }
    virtual void visitBinExpr(BinExpr *expr) { visitExpr(expr); }
    virtual void visitTerExpr(TerExpr *expr) { visitExpr(expr); }
    virtual void visitCallExpr(CallExpr *expr) { visitExpr(expr); }
    virtual void visitId(Id *id) { visitExpr(id); }
    virtual void visitGetFieldExpr(GetFieldExpr *expr) { visitExpr(expr); }
    virtual void visitSetFieldExpr(SetFieldExpr *expr) { visitExpr(expr); }
    virtual void visitGetTableExpr(GetTableExpr *expr) { visitExpr(expr); }
    virtual void visitSetTableExpr(SetTableExpr *expr) { visitExpr(expr); }
    virtual void visitBaseExpr(BaseExpr *expr) { visitExpr(expr); }
    virtual void visitRootExpr(RootExpr *expr) { visitExpr(expr); }
    virtual void visitThisExpr(ThisExpr *expr) { visitExpr(expr); }
    virtual void visitCalleeExpr(CalleeExpr *expr) { visitExpr(expr); }
    virtual void visitLiteralExpr(LiteralExpr *expr) { visitExpr(expr); }
    virtual void visitIncExpr(IncExpr *expr) { visitExpr(expr); }
    virtual void visitDeclExpr(DeclExpr *expr) { visitExpr(expr); }
    virtual void visitArrayExpr(ArrayExpr *expr) { visitExpr(expr); }
    virtual void visitCommaExpr(CommaExpr *expr) { visitExpr(expr); }

    virtual void visitStmt(Statement *stmt) { visitNode(stmt); }
    virtual void visitBlock(Block *block) { visitStmt(block); }
    virtual void visitIfStatement(IfStatement *ifstmt) { visitStmt(ifstmt); }
    virtual void visitLoopStatement(LoopStatement *loop) { visitStmt(loop); }
    virtual void visitWhileStatement(WhileStatement *loop) { visitLoopStatement(loop); }
    virtual void visitDoWhileStatement(DoWhileStatement *loop) { visitLoopStatement(loop); }
    virtual void visitForStatement(ForStatement *loop) { visitLoopStatement(loop); }
    virtual void visitForeachStatement(ForeachStatement *loop) { visitLoopStatement(loop); }
    virtual void visitSwitchStatement(SwitchStatement *swtch) { visitStmt(swtch); }
    virtual void visitTryStatement(TryStatement *tr) { visitStmt(tr); }
    virtual void visitTerminateStatement(TerminateStatement *term) { visitStmt(term); }
    virtual void visitReturnStatement(ReturnStatement *ret) { visitTerminateStatement(ret); }
    virtual void visitYieldStatement(YieldStatement *yld) { visitTerminateStatement(yld); }
    virtual void visitThrowStatement(ThrowStatement *thr) { visitTerminateStatement(thr); }
    virtual void visitJumpStatement(JumpStatement *jmp) { visitStmt(jmp); }
    virtual void visitBreakStatement(BreakStatement *jmp) { visitJumpStatement(jmp); }
    virtual void visitContinueStatement(ContinueStatement *jmp) { visitJumpStatement(jmp); }
    virtual void visitExprStatement(ExprStatement *estmt) { visitStmt(estmt); }
    virtual void visitEmptyStatement(EmptyStatement *empty) { visitStmt(empty); }

    virtual void visitDecl(Decl *decl) { visitStmt(decl); }
    virtual void visitValueDecl(ValueDecl *decl) { visitDecl(decl); }
    virtual void visitVarDecl(VarDecl *decl) { visitValueDecl(decl); }
    virtual void visitParamDecl(ParamDecl *decl) { visitValueDecl(decl); }
    virtual void visitTableDecl(TableDecl *tbl) { visitDecl(tbl); }
    virtual void visitClassDecl(ClassDecl *cls) { visitTableDecl(cls); }
    virtual void visitFunctionDecl(FunctionDecl *f) { visitDecl(f); }
    virtual void visitConstructorDecl(ConstructorDecl *ctr) { visitFunctionDecl(ctr); }
    virtual void visitConstDecl(ConstDecl *cnst) { visitDecl(cnst); }
    virtual void visitEnumDecl(EnumDecl *enm) { visitDecl(enm); }
    virtual void visitDeclGroup(DeclGroup *grp) { visitDecl(grp); }
    virtual void visitDesctructionDecl(DesctructionDecl *destruct) { visitDecl(destruct); }
};

#endif // _SQAST_H_