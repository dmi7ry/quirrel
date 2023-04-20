#include "sqast.h"


//void Node::visit(Visitor *visitor) {
//    switch (op())
//    {
//    case TO_BLOCK:      visitor->visitBlock(static_cast<Block *>(this)); return;
//    case TO_IF:         visitor->visitIfStatement(static_cast<IfStatement *>(this)); return;
//    case TO_WHILE:      visitor->visitWhileStatement(static_cast<WhileStatement *>(this)); return;
//    case TO_DOWHILE:    visitor->visitDoWhileStatement(static_cast<DoWhileStatement *>(this)); return;
//    case TO_FOR:        visitor->visitForStatement(static_cast<ForStatement *>(this)); return;
//    case TO_FOREACH:    visitor->visitForeachStatement(static_cast<ForeachStatement *>(this)); return;
//    case TO_SWITCH:     visitor->visitSwitchStatement(static_cast<SwitchStatement *>(this)); return;
//    case TO_RETURN:     visitor->visitReturnStatement(static_cast<ReturnStatement *>(this)); return;
//    case TO_YIELD:      visitor->visitYieldStatement(static_cast<YieldStatement *>(this)); return;
//    case TO_THROW:      visitor->visitThrowStatement(static_cast<ThrowStatement *>(this)); return;
//    case TO_TRY:        visitor->visitTryStatement(static_cast<TryStatement *>(this)); return;
//    case TO_BREAK:      visitor->visitBreakStatement(static_cast<BreakStatement *>(this)); return;
//    case TO_CONTINUE:   visitor->visitContinueStatement(static_cast<ContinueStatement *>(this)); return;
//    case TO_EXPR_STMT:  visitor->visitExprStatement(static_cast<ExprStatement *>(this)); return;
//    case TO_EMPTY:      visitor->visitEmptyStatement(static_cast<EmptyStatement *>(this)); return;
//    //case TO_STATEMENT_MARK:
//    case TO_ID:         visitor->visitId(static_cast<Id *>(this)); return;
//    case TO_COMMA:      visitor->visitCommaExpr(static_cast<CommaExpr *>(this)); return;
//    case TO_NULLC:
//    case TO_ASSIGN:
//    case TO_OROR:
//    case TO_ANDAND:
//    case TO_OR:
//    case TO_XOR:
//    case TO_AND:
//    case TO_NE:
//    case TO_EQ:
//    case TO_3CMP:
//    case TO_GE:
//    case TO_GT:
//    case TO_LE:
//    case TO_LT:
//    case TO_IN:
//    case TO_INSTANCEOF:
//    case TO_USHR:
//    case TO_SHR:
//    case TO_SHL:
//    case TO_MUL:
//    case TO_DIV:
//    case TO_MOD:
//    case TO_ADD:
//    case TO_SUB: 
//    case TO_NEWSLOT:
//    case TO_INEXPR_ASSIGN:
//    case TO_PLUSEQ:
//    case TO_MINUSEQ:
//    case TO_MULEQ:
//    case TO_DIVEQ:
//    case TO_MODEQ:
//        visitor->visitBinExpr(static_cast<BinExpr *>(this)); return;
//    case TO_NOT:
//    case TO_BNOT:
//    case TO_NEG:
//    case TO_TYPEOF:
//    case TO_RESUME:
//    case TO_CLONE:
//    case TO_PAREN:
//    case TO_DELETE:
//        visitor->visitUnExpr(static_cast<UnExpr *>(this)); return;
//    case TO_LITERAL:
//        visitor->visitLiteralExpr(static_cast<LiteralExpr *>(this)); return;
//    case TO_BASE:
//        visitor->visitBaseExpr(static_cast<BaseExpr *>(this)); return;
//    case TO_ROOT:
//        visitor->visitRootExpr(static_cast<RootExpr *>(this)); return;
//    //case TO_THIS:
//    //case TO_CALLEE:
//    case TO_INC:
//        visitor->visitIncExpr(static_cast<IncExpr *>(this)); return;
//    case TO_DECL_EXPR:
//        visitor->visitDeclExpr(static_cast<DeclExpr *>(this)); return;
//    case TO_ARRAYEXPR:
//        visitor->visitArrayExpr(static_cast<ArrayExpr *>(this)); return;
//    case TO_GETFIELD:
//        visitor->visitGetFieldExpr(static_cast<GetFieldExpr *>(this)); return;
//    case TO_SETFIELD:
//        visitor->visitSetFieldExpr(static_cast<SetFieldExpr *>(this)); return;
//    case TO_GETTABLE:
//        visitor->visitGetTableExpr(static_cast<GetTableExpr *>(this)); return;
//    case TO_SETTABLE:
//        visitor->visitSetTableExpr(static_cast<SetTableExpr *>(this)); return;
//    case TO_CALL:
//        visitor->visitCallExpr(static_cast<CallExpr *>(this)); return;
//    case TO_TERNARY:
//        visitor->visitTerExpr(static_cast<TerExpr *>(this)); return;
//    //case TO_EXPR_MARK:
//    case TO_VAR:
//        visitor->visitVarDecl(static_cast<VarDecl *>(this)); return;
//    case TO_PARAM:
//        visitor->visitParamDecl(static_cast<ParamDecl *>(this)); return;
//    case TO_CONST:
//        visitor->visitConstDecl(static_cast<ConstDecl *>(this)); return;
//    case TO_DECL_GROUP:
//        visitor->visitDeclGroup(static_cast<DeclGroup *>(this)); return;
//    case TO_DESTRUCT:
//        visitor->visitDesctructionDecl(static_cast<DesctructionDecl *>(this)); return;
//    case TO_FUNCTION:
//        visitor->visitFunctionDecl(static_cast<FunctionDecl *>(this)); return;
//    case TO_CONSTRUCTOR:
//        visitor->visitConstructorDecl(static_cast<ConstructorDecl *>(this)); return;
//    case TO_CLASS:
//        visitor->visitClassDecl(static_cast<ClassDecl *>(this)); return;
//    case TO_ENUM:
//        visitor->visitEnumDecl(static_cast<EnumDecl *>(this)); return;
//    case TO_TABLE:
//        visitor->visitTableDecl(static_cast<TableDecl *>(this)); return;
//    default:
//        break;
//    }
//}

void Node::visitChildren(Visitor *visitor) {
    switch (op())
    {
    case TO_BLOCK:      static_cast<Block *>(this)->visitChildren(visitor); return;
    case TO_IF:         static_cast<IfStatement *>(this)->visitChildren(visitor); return;
    case TO_WHILE:      static_cast<WhileStatement *>(this)->visitChildren(visitor); return;
    case TO_DOWHILE:    static_cast<DoWhileStatement *>(this)->visitChildren(visitor); return;
    case TO_FOR:        static_cast<DoWhileStatement *>(this)->visitChildren(visitor); return;
    case TO_FOREACH:    static_cast<ForeachStatement *>(this)->visitChildren(visitor); return;
    case TO_SWITCH:     static_cast<SwitchStatement *>(this)->visitChildren(visitor); return;
    case TO_RETURN:     static_cast<ReturnStatement *>(this)->visitChildren(visitor); return;
    case TO_YIELD:      static_cast<YieldStatement *>(this)->visitChildren(visitor); return;
    case TO_THROW:      static_cast<ThrowStatement *>(this)->visitChildren(visitor); return;
    case TO_TRY:        static_cast<TryStatement *>(this)->visitChildren(visitor); return;
    case TO_BREAK:      static_cast<BreakStatement *>(this)->visitChildren(visitor); return;
    case TO_CONTINUE:   static_cast<ContinueStatement *>(this)->visitChildren(visitor); return;
    case TO_EXPR_STMT:  static_cast<ExprStatement *>(this)->visitChildren(visitor); return;
    case TO_EMPTY:      static_cast<EmptyStatement *>(this)->visitChildren(visitor); return;
        //case TO_STATEMENT_MARK:
    case TO_ID:         static_cast<Id *>(this)->visitChildren(visitor); return;
    case TO_COMMA:      static_cast<CommaExpr *>(this)->visitChildren(visitor); return;
    case TO_NULLC:
    case TO_ASSIGN:
    case TO_OROR:
    case TO_ANDAND:
    case TO_OR:
    case TO_XOR:
    case TO_AND:
    case TO_NE:
    case TO_EQ:
    case TO_3CMP:
    case TO_GE:
    case TO_GT:
    case TO_LE:
    case TO_LT:
    case TO_IN:
    case TO_INSTANCEOF:
    case TO_USHR:
    case TO_SHR:
    case TO_SHL:
    case TO_MUL:
    case TO_DIV:
    case TO_MOD:
    case TO_ADD:
    case TO_SUB:
    case TO_NEWSLOT:
    case TO_INEXPR_ASSIGN:
    case TO_PLUSEQ:
    case TO_MINUSEQ:
    case TO_MULEQ:
    case TO_DIVEQ:
    case TO_MODEQ:
        static_cast<BinExpr *>(this)->visitChildren(visitor); return;
    case TO_NOT:
    case TO_BNOT:
    case TO_NEG:
    case TO_TYPEOF:
    case TO_RESUME:
    case TO_CLONE:
    case TO_PAREN:
    case TO_DELETE:
        static_cast<UnExpr *>(this)->visitChildren(visitor); return;
    case TO_LITERAL:
        static_cast<LiteralExpr *>(this)->visitChildren(visitor); return;
    case TO_BASE:
        static_cast<BaseExpr *>(this)->visitChildren(visitor); return;
    case TO_ROOT:
        static_cast<RootExpr *>(this)->visitChildren(visitor); return;
    case TO_INC:
        static_cast<IncExpr *>(this)->visitChildren(visitor); return;
    case TO_DECL_EXPR:
        static_cast<DeclExpr *>(this)->visitChildren(visitor); return;
    case TO_ARRAYEXPR:
        static_cast<ArrayExpr *>(this)->visitChildren(visitor); return;
    case TO_GETFIELD:
        static_cast<GetFieldExpr *>(this)->visitChildren(visitor); return;
    case TO_SETFIELD:
        static_cast<SetFieldExpr *>(this)->visitChildren(visitor); return;
    case TO_GETTABLE:
        static_cast<GetTableExpr *>(this)->visitChildren(visitor); return;
    case TO_SETTABLE:
        static_cast<SetTableExpr *>(this)->visitChildren(visitor); return;
    case TO_CALL:
        static_cast<CallExpr *>(this)->visitChildren(visitor); return;
    case TO_TERNARY:
        static_cast<TerExpr *>(this)->visitChildren(visitor); return;
        //case TO_EXPR_MARK:
    case TO_VAR:
        static_cast<VarDecl *>(this)->visitChildren(visitor); return;
    case TO_PARAM:
        static_cast<ParamDecl *>(this)->visitChildren(visitor); return;
    case TO_CONST:
        static_cast<ConstDecl *>(this)->visitChildren(visitor); return;
    case TO_DECL_GROUP:
        static_cast<DeclGroup *>(this)->visitChildren(visitor); return;
    case TO_DESTRUCT:
        static_cast<DesctructionDecl *>(this)->visitChildren(visitor); return;
    case TO_FUNCTION:
        static_cast<FunctionDecl *>(this)->visitChildren(visitor); return;
    case TO_CONSTRUCTOR:
        static_cast<ConstructorDecl *>(this)->visitChildren(visitor); return;
    case TO_CLASS:
        static_cast<ClassDecl *>(this)->visitChildren(visitor); return;
    case TO_ENUM:
        static_cast<EnumDecl *>(this)->visitChildren(visitor); return;
    case TO_TABLE:
        static_cast<TableDecl *>(this)->visitChildren(visitor); return;
    default:
        break;
    }
}

void UnExpr::visitChildren(Visitor *visitor) { _arg->visit(visitor); }

void BinExpr::visitChildren(Visitor *visitor) {
    _lhs->visit(visitor);
    _rhs->visit(visitor);
}

void TerExpr::visitChildren(Visitor *visitor) {
    _a->visit(visitor);
    _b->visit(visitor);
    _c->visit(visitor);
}

void GetFieldExpr::visitChildren(Visitor *visitor) {
    receiver()->visit(visitor);
}

void SetFieldExpr::visitChildren(Visitor *visitor) {
    receiver()->visit(visitor);
    value()->visit(visitor);
}

void GetTableExpr::visitChildren(Visitor *visitor) {
    receiver()->visit(visitor);
    key()->visit(visitor);
}

void SetTableExpr::visitChildren(Visitor *visitor) {
    receiver()->visit(visitor);
    key()->visit(visitor);
    value()->visit(visitor);
}

void IncExpr::visitChildren(Visitor *visitor) { _arg->visit(visitor); }

void DeclExpr::visitChildren(Visitor *visitor) { _decl->visit(visitor); }

void CallExpr::visitChildren(Visitor *visitor) {
    visitor->visitExpr(_callee);
    for (int i = 0; i < _args.size(); ++i) _args[i]->visit(visitor);
}

void ArrayExpr::visitChildren(Visitor *visitor) {
    for (int i = 0; i < _inits.size(); ++i) _inits[i]->visit(visitor);
}

void CommaExpr::visitChildren(Visitor *visitor) {
    for (int i = 0; i < _exprs.size(); ++i) _exprs[i]->visit(visitor);
}

void ValueDecl::visitChildren(Visitor *visitor) {
    visitor->visitId(_name);
    if (_expr) _expr->visit(visitor);
}

void TableDecl::visitChildren(Visitor *visitor) {
    for (int i = 0; i < _members.size(); ++i) {
        _members[i].key->visit(visitor);
        _members[i].value->visit(visitor);
    }
}

void ClassDecl::visitChildren(Visitor *visitor)  {
    if (_key) _key->visit(visitor);
    if (_base) _base->visit(visitor);
    TableDecl::visitChildren(visitor);
}

void FunctionDecl::visitChildren(Visitor *visitor) {
    if (_name) visitor->visitId(_name);
    visitor->visitStmt(_body);
    for (int i = 0; i < _parameters.size(); ++i) {
        visitor->visitParamDecl(_parameters[i]);
    }
}

void EnumDecl::visitChildren(Visitor *visitor) {
    visitor->visitId(_id);
    for (int i = 0; i < _consts.size(); ++i) {
        visitor->visitId(_consts[i].id);
    }
}

void ConstDecl::visitChildren(Visitor *visitor) {
    visitor->visitId(_id);
}

void DeclGroup::visitChildren(Visitor *visitor) {
    for (int i = 0; i < _decls.size(); ++i) {
        _decls[i]->visit(visitor);
    }
}

void DesctructionDecl::visitChildren(Visitor *visitor) {
    DeclGroup::visitChildren(visitor);
    _expr->visit(visitor);
}

void Block::visitChildren(Visitor *visitor) {
    for (int i = 0; i < _statements.size(); ++i) {
        _statements[i]->visit(visitor);
    }
}

void IfStatement::visitChildren(Visitor *visitor) {
    _cond->visit(visitor);
    _thenB->visit(visitor);
    if (_elseB) _elseB->visit(visitor);
}

void LoopStatement::visitChildren(Visitor *visitor) {
    _body->visit(visitor);
}

void WhileStatement::visitChildren(Visitor *visitor) {
    _cond->visit(visitor);
    LoopStatement::visitChildren(visitor);
}

void DoWhileStatement::visitChildren(Visitor *visitor) {
    LoopStatement::visitChildren(visitor);
    _cond->visit(visitor);
}

void ForStatement::visitChildren(Visitor *visitor) {
    if (_init) _init->visit(visitor);
    if (_cond) _cond->visit(visitor);
    if (_mod) _mod->visit(visitor);

    LoopStatement::visitChildren(visitor);
}


void ForeachStatement::visitChildren(Visitor *visitor) {
    if (_idx) visitor->visitNode(_idx);
    if (_val) visitor->visitExpr(_val);
    if (_container) _container->visit(visitor);

    LoopStatement::visitChildren(visitor);
}

void SwitchStatement::visitChildren(Visitor *visitor) {
    _expr->visit(visitor);
    for (int i = 0; i < _cases.size(); ++i) {
        _cases[i].val->visit(visitor);
        _cases[i].stmt->visit(visitor);
    }
    if (_defaultCase.stmt) {
        _defaultCase.stmt->visit(visitor);
    }
}

void TryStatement::visitChildren(Visitor *visitor) {
    _tryStmt->visit(visitor);
    visitor->visitId(_exception);
    _catchStmt->visit(visitor);
}

void TerminateStatement::visitChildren(Visitor *visitor) {
    if (_arg) _arg->visit(visitor);
}


void ExprStatement::visitChildren(Visitor *visitor) { _expr->visit(visitor); }

