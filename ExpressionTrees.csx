using System;
using System.Collections.Generic;
using System.Linq.Expressions;

// I.
// https://stackoverflow.com/questions/44408583/linq-query-to-check-for-a-predicate-in-all-columns-in-a-table/

class Book
{
    public string Title { get; set; }

    public Publisher Publisher { get; set; }

    public Author[] Authors { get; set; }
}

class Publisher
{
    public string Title { get; set; }
}

class Author
{
    public string GivenName { get; set; }

    public string FamilyName { get; set; }
}

Expression MakeOrExpression(IEnumerable<Expression> expressions)
{
    Expression result = Expression.Constant(false);

    foreach (var expression in expressions)
        result = Expression.MakeBinary(ExpressionType.Or, result, expression);

    return result;
}

IEnumerable<Expression> GetPropertyExpressions(Expression parameter, ConstantExpression constant)
{
    foreach (var property in parameter.Type.GetProperties())
    {
        if (property.PropertyType == typeof(string))
            yield return Expression.Equal(Expression.Property(parameter, property), constant);
        else if (property.PropertyType.BaseType == typeof(Array))
        {
            var elementType = property.PropertyType.GetElementType()!;
            var genericAnyMethod = typeof(Enumerable)
                .GetMethods()
                .Single(x => x.Name == "Any" && x.GetParameters().Length == 2)
                .MakeGenericMethod(new[] { elementType });
            var subParameter = Expression.Parameter(elementType);
            var orExpression = MakeOrExpression(GetPropertyExpressions(subParameter, constant));
            var lambda = Expression.Lambda(orExpression, subParameter);

            yield return Expression.Call(null,
                genericAnyMethod,
                Expression.Property(parameter, property),
                lambda);
        }
        else if (property.PropertyType.Namespace == null
            || !property.PropertyType.Namespace.StartsWith("System"))
        {
            var subProperty = Expression.Property(parameter, property);
            foreach (var expression in GetPropertyExpressions(subProperty, constant))
                yield return expression;
        }
    }
}

Expression<Func<T, bool>> MakeSearchExpression<T>(string value)
{
    var constant = Expression.Constant(value);
    var bookParameter = Expression.Parameter(typeof(T));
    var orExpression = MakeOrExpression(GetPropertyExpressions(bookParameter, constant));

    return Expression.Lambda<Func<T, bool>>(orExpression, bookParameter);
}

WriteLine(MakeSearchExpression<Book>("Shakespeare"));

// II.

class DerivativeVisitor : ExpressionVisitor
{
    protected override Expression VisitConstant(ConstantExpression node)
    {
        return Expression.Constant(0.0);
    }

    protected override Expression VisitParameter(ParameterExpression node)
    {
        return Expression.Constant(1.0);
    }

    protected override Expression VisitMember(MemberExpression node)
    {
        return Expression.Constant(0.0);
    }

    protected override Expression VisitBinary(BinaryExpression node)
    {
        switch (node.NodeType)
        {
            case ExpressionType.Add:
                return Expression.Add(Visit(node.Left), Visit(node.Right));

            case ExpressionType.Multiply:
                var addend = Expression.Multiply(Visit(node.Left), node.Right);
                var augend = Expression.Multiply(node.Left, Visit(node.Right));
                return Expression.Add(addend, augend);

            default:
                throw new InvalidOperationException("Binary operator is not applicable for derivative");
        }
    }
}

LambdaExpression Derivative(Expression<Func<double, double>> f)
{
    var derivativeVisitor = new DerivativeVisitor();

    return Expression.Lambda(derivativeVisitor.Visit(f.Body), f.Parameters);
}

var a = 15.0;
Derivative(x => a * x * (x + 3)).ToString()

using System.Diagnostics;

class DerivativeVisitor : ExpressionVisitor
{
    protected override Expression VisitConstant(ConstantExpression node)
    {
        return Expression.Constant(0.0);
    }

    protected override Expression VisitParameter(ParameterExpression node)
    {
        return Expression.Constant(1.0);
    }

    protected override Expression VisitMember(MemberExpression node)
    {
        return Expression.Constant(0.0);
    }

    protected override Expression VisitBinary(BinaryExpression node)
    {
        switch (node.NodeType)
        {
            case ExpressionType.Add:
                return Add(Visit(node.Left), Visit(node.Right));

            case ExpressionType.Multiply:
                var addend = Multiply(Visit(node.Left), node.Right);
                var augend = Multiply(node.Left, Visit(node.Right));
                return Add(addend, augend);

            default:
                throw new InvalidOperationException("Binary operator is not applicable for derivative");
        }
    }

    private bool IsEqual(Expression e, double value)
    {
        if (!(e is ConstantExpression eConst))
            return false;

        if (eConst.Type != typeof(double))
            return false;

        Debug.Assert(eConst.Value != null);

        return (double)eConst.Value == value;
    }

    private Expression Add(Expression a, Expression b)
    {
        if (IsEqual(a, 0.0))
            return b;

        if (IsEqual(b, 0.0))
            return a;

        return Expression.Add(a, b);
    }

    private Expression Multiply(Expression a, Expression b)
    {
        if (IsEqual(a, 0.0))
            return Expression.Constant(0.0);

        if (IsEqual(b, 0.0))
            return Expression.Constant(0.0);

        if (IsEqual(a, 1.0))
            return b;

        if (IsEqual(b, 1.0))
            return a;

        return Expression.Multiply(a, b);
    }
}

LambdaExpression Derivative(Expression<Func<double, double>> f)
{
    var derivativeVisitor = new DerivativeVisitor();

    return Expression.Lambda(derivativeVisitor.Visit(f.Body), f.Parameters);
}

Derivative(x => a * x * (x + 3)).ToString()

// SySharp
// https://www.nuget.org/packages/SySharp/
// https://github.com/markshevchenko/sysharp
