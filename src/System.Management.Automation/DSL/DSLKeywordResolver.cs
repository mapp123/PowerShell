/********************************************************************++
Copyright (c) Microsoft Corporation.  All rights reserved.
--********************************************************************/

using System.Collections.Generic;
using System.Linq;

namespace System.Management.Automation.Language
{
    internal class DslKeywordTable : ParsingBaseTable
    {
        internal bool HadError;
        internal TypeDefinitionAst CurrentTypeDef;
        internal List<Tuple<TypeDefinitionAst, IScriptExtent>> NestedKeywords;
        internal DynamicKeyword CurrentKeyword;

        internal DslKeywordTable(Parser parser) : base(parser) { }

        internal override TypeLookupResult LookupType(TypeName typeName)
        {
            // Look up type only in the current scope
            TypeLookupResult result = _scopes[_scopes.Count - 1].LookupType(typeName);
            return result;
        }
    }

    internal class DslKeywordResolver : AstVisitor2, IAstPostVisitHandler
    {
        private readonly DslKeywordResolvePostActionVisitor _dslKeywordResolvePostActionVisitor;
        internal readonly DslKeywordTable _keywordTable;
        internal readonly Parser _parser;

        private DslKeywordResolver(Parser parser)
        {
            _keywordTable = new DslKeywordTable(parser);
            _parser = parser;
            _dslKeywordResolvePostActionVisitor = new DslKeywordResolvePostActionVisitor { _dslKeywordResolver = this };
        }

        internal static void ResolveKeywords(Parser parser, ScriptBlockAst scriptBlockAst)
        {
            Diagnostics.Assert(scriptBlockAst.Parent == null, "Can only resolve starting from the root");

            var resolver = new DslKeywordResolver(parser);
            resolver._keywordTable.EnterScope(scriptBlockAst, ScopeType.ScriptBlock);
            scriptBlockAst.Visit(resolver);
            resolver._keywordTable.LeaveScope();

            Diagnostics.Assert(resolver._keywordTable._scopes.Count == 0, "Somebody missed removing a scope");
        }

        private bool IsConstant<T>(Ast ast, out T value)
        {
            if (IsConstantValueVisitor.IsConstant(ast, out object constantValue, forAttribute: false, forRequires: false))
            {
                value = (T) constantValue;
                return true;
            }

            value = default(T);
            _parser.ReportError(ast.Extent, () => "Cannot evaluate argument at parsing time.");
            _keywordTable.HadError = true;
            return false;
        }

        public void PostVisit(Ast ast)
        {
            ast.Accept(_dslKeywordResolvePostActionVisitor);
        }

        public override AstVisitAction VisitScriptBlockExpression(ScriptBlockExpressionAst scriptBlockExpressionAst)
        {
            return AstVisitAction.SkipChildren;
        }

        public override AstVisitAction VisitFunctionDefinition(FunctionDefinitionAst functionDefinitionAst)
        {
            return AstVisitAction.SkipChildren;
        }

        public override AstVisitAction VisitUsingStatement(UsingStatementAst usingStatementAst)
        {
            // Maybe import all DSLs from the using module in future
            return AstVisitAction.Continue;
        }

        public override AstVisitAction VisitTypeDefinition(TypeDefinitionAst typeDefinitionAst)
        {
            var keywordBaseType = typeDefinitionAst.BaseTypes.FirstOrDefault((baseType) => baseType.TypeName.GetReflectionType() == typeof(Keyword));
            var keywordAttribute = typeDefinitionAst.Attributes.FirstOrDefault((attribute) => attribute.TypeName.GetReflectionAttributeType() == typeof(KeywordAttribute));
            var nestedKeywordAttribute = typeDefinitionAst.Attributes.FirstOrDefault((attribute) => attribute.TypeName.GetReflectionAttributeType() == typeof(NestedKeywordsAttribute));

            if (keywordBaseType != null && keywordAttribute == null)
            {
                _parser.ReportError(keywordBaseType.Extent, () => "A DSL keyword requires 'KeywordAttribute' to be declared.");
                _keywordTable.HadError = true;
            }
            if (keywordBaseType == null && keywordAttribute != null)
            {
                _parser.ReportError(keywordAttribute.Extent, () => "A DSL keyword needs to derive from 'Keyword'.");
                _keywordTable.HadError = true;
            }

            if (_keywordTable.HadError) { return AstVisitAction.SkipChildren; }

            _keywordTable.CurrentKeyword = new DynamicKeyword() { Keyword = typeDefinitionAst.Name };
            foreach (var namedArg in keywordAttribute.NamedArguments)
            {
                var argument = namedArg.Argument;
                switch (namedArg.ArgumentName)
                {
                    case nameof(KeywordAttribute.NameMode):
                        if (IsConstant(argument, out DynamicKeywordNameMode nameMode))
                        {
                            _keywordTable.CurrentKeyword.NameMode = nameMode;
                        }
                        break;

                    case nameof(KeywordAttribute.BodyMode):
                        if (IsConstant(argument, out DynamicKeywordBodyMode bodyMode))
                        {
                            _keywordTable.CurrentKeyword.BodyMode = bodyMode;
                        }
                        break;
                    
                    case nameof(KeywordAttribute.DirectCall):
                        if (IsConstant(argument, out bool directCall))
                        {
                            _keywordTable.CurrentKeyword.DirectCall = directCall;
                        }
                        break;
                    default: break;
                }
            }

            if (nestedKeywordAttribute != null)
            {
                foreach (var posiArg in nestedKeywordAttribute.PositionalArguments)
                {
                    TypeExpressionAst typeExpr = posiArg as TypeExpressionAst;
                    if (typeExpr == null)
                    {
                        _parser.ReportError(posiArg.Extent, () => "Argument should represent a TypeExpression");
                        _keywordTable.HadError = true;
                        continue;
                    }
                    var typeName = typeExpr.TypeName as TypeName;
                    if (typeName == null)
                    {
                        _parser.ReportError(posiArg.Extent, () => "Argument should not be an array type or a generic type.");
                        _keywordTable.HadError = true;
                        continue;
                    }
                    var typeLookupResult = _keywordTable.LookupType(typeName);
                    if (typeLookupResult == null)
                    {
                        _parser.ReportError(posiArg.Extent, () => "Cannot find the nested keyword");
                    }
                    if (_keywordTable.NestedKeywords == null)
                    {
                        _keywordTable.NestedKeywords = new List<Tuple<TypeDefinitionAst, IScriptExtent>>();
                    }
                    _keywordTable.NestedKeywords.Add(Tuple.Create(typeLookupResult.Type, posiArg.Extent));
                }
            }

            return AstVisitAction.Continue;
        }

        public override AstVisitAction VisitPropertyMember(PropertyMemberAst propertyMemberAst)
        {
            return AstVisitAction.SkipChildren;
        }

        public override AstVisitAction VisitTypeExpression(TypeExpressionAst typeExpressionAst)
        {
            //DispatchTypeName(typeExpressionAst.TypeName, genericArgumentCount: 0, isAttribute: false);
            return AstVisitAction.Continue;
        }

        public override AstVisitAction VisitAttribute(AttributeAst attributeAst)
        {
            // Do something
            return AstVisitAction.Continue;
        }
    }

    internal class DslKeywordResolvePostActionVisitor : DefaultCustomAstVisitor2
    {
        internal DslKeywordResolver _dslKeywordResolver;

        public override object VisitTypeDefinition(TypeDefinitionAst typeDefinitionAst)
        {
            return null;
        }
    }
}