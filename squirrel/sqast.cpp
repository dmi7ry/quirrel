#include "sqast.h"


void Expr::visit(Visitor &visitor) { visitor.visitExpr(this); }

void Id::visit(Visitor &visitor) { 
    visitor.visitId(this);
}
void Id::visitChildren(Visitor &visitor) { }

void UnExpr::visit(Visitor &visitor) { visitor.visitUnExpr(this); }
void UnExpr::visitChildren(Visitor &visitor) { _arg->visit(visitor); }

void BinExpr::visit(Visitor &visitor) { visitor.visitBinExpr(this); }
void BinExpr::visitChildren(Visitor &visitor) {
    _lhs->visit(visitor);
    _rhs->visit(visitor);
}

void TerExpr::visit(Visitor &visitor)  { visitor.visitTerExpr(this); }
void TerExpr::visitChildren(Visitor &visitor) {
    _a->visit(visitor);
    _b->visit(visitor);
    _c->visit(visitor);
}

void GetFieldExpr::visit(Visitor &visitor){ visitor.visitGetFieldExpr(this); }
void GetFieldExpr::visitChildren(Visitor &visitor) {
    receiver()->visit(visitor);
}

void SetFieldExpr::visit(Visitor &visitor) { visitor.visitSetFieldExpr(this); }
void SetFieldExpr::visitChildren(Visitor &visitor) {
    receiver()->visit(visitor);
    _value->visit(visitor);
}

void GetTableExpr::visit(Visitor &visitor) { visitor.visitGetTableExpr(this); }
void GetTableExpr::visitChildren(Visitor &visitor) {
    receiver()->visit(visitor);
    key()->visit(visitor);
}

void SetTableExpr::visit(Visitor &visitor) { visitor.visitSetTableExpr(this); }
void SetTableExpr::visitChildren(Visitor &visitor) {
    receiver()->visit(visitor);
    key()->visit(visitor);
    _val->visit(visitor);
}

void BaseExpr::visit(Visitor &visitor) { visitor.visitBaseExpr(this); }
void BaseExpr::visitChildren(Visitor &visitor) { }

void RootExpr::visit(Visitor &visitor) { visitor.visitRootExpr(this); }
void RootExpr::visitChildren(Visitor &visitor)  { }

void ThisExpr::visit(Visitor &visitor) { visitor.visitThisExpr(this); }
void ThisExpr::visitChildren(Visitor &visitor) { }

void CalleeExpr::visit(Visitor &visitor)  { visitor.visitCalleeExpr(this); }
void CalleeExpr::visitChildren(Visitor &visitor) { }

void LiteralExpr::visit(Visitor &visitor) { visitor.visitLiteralExpr(this); }
void LiteralExpr::visitChildren(Visitor &visitor) { }

void IncExpr::visit(Visitor &visitor) { visitor.visitIncExpr(this); }
void IncExpr::visitChildren(Visitor &visitor) { _arg->visit(visitor); }

void DeclExpr::visit(Visitor &visitor) { visitor.visitDeclExpr(this); }
void DeclExpr::visitChildren(Visitor &visitor) { _decl->visit(visitor); }

void CallExpr::visit(Visitor &visitor) { visitor.visitCallExpr(this); }
void CallExpr::visitChildren(Visitor &visitor) {
    visitor.visitExpr(_callee);
    for (int i = 0; i < _args.size(); ++i) _args[i]->visit(visitor);
}

void ArrayExpr::visit(Visitor &visitor) { visitor.visitArrayExpr(this); }
void ArrayExpr::visitChildren(Visitor &visitor) {
    for (int i = 0; i < _inits.size(); ++i) _inits[i]->visit(visitor);
}

void CommaExpr::visit(Visitor &visitor) { visitor.visitCommaExpr(this); }
void CommaExpr::visitChildren(Visitor &visitor) {
    for (int i = 0; i < _exprs.size(); ++i) _exprs[i]->visit(visitor);
}

void Statement::visit(Visitor &visitor) { visitor.visitStmt(this); }

void Decl::visit(Visitor &visitor) { visitor.visitDecl(this); }

void ValueDecl::visit(Visitor &visitor) { visitor.visitValueDecl(this); }
void ValueDecl::visitChildren(Visitor &visitor) {
    visitor.visitId(_name);
    if (_expr) _expr->visit(visitor);
}

void ParamDecl::visit(Visitor &visitor) { visitor.visitParamDecl(this); }

void VarDecl::visit(Visitor &visitor) { visitor.visitVarDecl(this); }

void TableDecl::visit(Visitor &visitor) { visitor.visitTableDecl(this); }
void TableDecl::visitChildren(Visitor &visitor) {
    for (int i = 0; i < _members.size(); ++i) {
        _members[i].key->visit(visitor);
        _members[i].value->visit(visitor);
    }
}

void ClassDecl::visit(Visitor &visitor) { visitor.visitClassDecl(this); }
void ClassDecl::visitChildren(Visitor &visitor)  {
    if (_key) _key->visit(visitor);
    if (_base) _base->visit(visitor);
    TableDecl::visitChildren(visitor);
}

void FunctionDecl::visit(Visitor &visitor) { visitor.visitFunctionDecl(this); }
void FunctionDecl::visitChildren(Visitor &visitor) {
    if (_name) visitor.visitId(_name);
    visitor.visitStmt(_body);
    for (int i = 0; i < _parameters.size(); ++i) {
        visitor.visitParamDecl(_parameters[i]);
    }
}

void ConstructorDecl::visit(Visitor &visitor) { visitor.visitConstructorDecl(this); }

void EnumDecl::visit(Visitor &visitor) { visitor.visitEnumDecl(this); }
void EnumDecl::visitChildren(Visitor &visitor) {
    visitor.visitId(_id);
    for (int i = 0; i < _consts.size(); ++i) {
        visitor.visitId(_consts[i].id);
    }
}

void ConstDecl::visit(Visitor &visitor) { visitor.visitConstDecl(this); }
void ConstDecl::visitChildren(Visitor &visitor) {
    visitor.visitId(_id);
}

void DeclGroup::visit(Visitor &visitor) { visitor.visitDeclGroup(this); }
void DeclGroup::visitChildren(Visitor &visitor) {
    for (int i = 0; i < _decls.size(); ++i) {
        _decls[i]->visit(visitor);
    }
}

void DesctructionDecl::visit(Visitor &visitor) { visitor.visitDesctructionDecl(this); }
void DesctructionDecl::visitChildren(Visitor &visitor) {
    DeclGroup::visitChildren(visitor);
    _expr->visit(visitor);
}

void Block::visit(Visitor &visitor) { visitor.visitBlock(this); }
void Block::visitChildren(Visitor &visitor) {
    for (int i = 0; i < _statements.size(); ++i) {
        _statements[i]->visit(visitor);
    }
}

void IfStatement::visit(Visitor &visitor) { visitor.visitIfStatement(this); }
void IfStatement::visitChildren(Visitor &visitor) {
    _cond->visit(visitor);
    _thenB->visit(visitor);
    if (_elseB) _elseB->visit(visitor);
}

void LoopStatement::visit(Visitor &visitor) { visitor.visitLoopStatement(this); }
void LoopStatement::visitChildren(Visitor &visitor) {
    _body->visit(visitor);
}

void WhileStatement::visit(Visitor &visitor) { visitor.visitWhileStatement(this); }
void WhileStatement::visitChildren(Visitor &visitor) {
    _cond->visit(visitor);
    LoopStatement::visitChildren(visitor);
}

void DoWhileStatement::visit(Visitor &visitor) { visitor.visitDoWhileStatement(this); }
void DoWhileStatement::visitChildren(Visitor &visitor) {
    LoopStatement::visitChildren(visitor);
    _cond->visit(visitor);
}

void ForStatement::visit(Visitor &visitor) { visitor.visitForStatement(this); }
void ForStatement::visitChildren(Visitor &visitor) {
    if (_init) _init->visit(visitor);
    if (_cond) _cond->visit(visitor);
    if (_mod) _mod->visit(visitor);

    LoopStatement::visitChildren(visitor);
}


void ForeachStatement::visit(Visitor &visitor) { visitor.visitForeachStatement(this); }
void ForeachStatement::visitChildren(Visitor &visitor) {
    if (_idx) visitor.visitNode(_idx);
    if (_val) visitor.visitExpr(_val);
    if (_container) _container->visit(visitor);

    LoopStatement::visitChildren(visitor);
}

void SwitchStatement::visit(Visitor &visitor) { visitor.visitSwitchStatement(this); }
void SwitchStatement::visitChildren(Visitor &visitor) {
    _expr->visit(visitor);
    for (int i = 0; i < _cases.size(); ++i) {
        _cases[i].val->visit(visitor);
        _cases[i].stmt->visit(visitor);
    }
    if (_defaultCase.stmt) {
        _defaultCase.stmt->visit(visitor);
    }
}

void TryStatement::visit(Visitor &visitor) { visitor.visitTryStatement(this); }
void TryStatement::visitChildren(Visitor &visitor) {
    _tryStmt->visit(visitor);
    visitor.visitId(_exception);
    _catchStmt->visit(visitor);
}

void TerminateStatement::visit(Visitor &visitor) { visitor.visitTerminateStatement(this); }
void TerminateStatement::visitChildren(Visitor &visitor) {
    if (_arg) _arg->visit(visitor);
}

void ReturnStatement ::visit(Visitor &visitor) { visitor.visitReturnStatement(this); }

void YieldStatement::visit(Visitor &visitor) { visitor.visitYieldStatement(this); }

void ThrowStatement::visit(Visitor &visitor) { visitor.visitThrowStatement(this); }

void JumpStatement::visit(Visitor &visitor) { visitor.visitJumpStatement(this); }
void JumpStatement::visitChildren(Visitor &visitor) {}

void BreakStatement::visit(Visitor &visitor) { visitor.visitBreakStatement(this); }

void ContinueStatement::visit(Visitor &visitor) { visitor.visitContinueStatement(this); }

void ExprStatement::visit(Visitor &visitor) { visitor.visitExprStatement(this); }
void ExprStatement::visitChildren(Visitor &visitor) { _expr->visit(visitor); }

void EmptyStatement::visit(Visitor &visitor) { visitor.visitEmptyStatement(this); }
void EmptyStatement::visitChildren(Visitor &visitor) {}