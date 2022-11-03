using System;
using System.Collections.Generic;
using System.Linq.Expressions;

// https://stackoverflow.com/questions/44408583/linq-query-to-check-for-a-predicate-in-all-columns-in-a-table/

class Book
{
    public string Title { get; set; }

    public Publisher Publisher { get; set; }

    public Author[] Authors { get; set; }
    
    public override string ToString()
    {
        return string.Join(", ", Authors.Select(a => a.GivenName + " " + a.FamilyName))
             + ", " + Title
             + " (Издательство " + Publisher.Title + ")";
    }
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

Expression MakeOrBinary(Expression a, Expression b) =>
    Expression.MakeBinary(ExpressionType.Or, a, b);

Expression MakeOrExpression(IEnumerable<Expression> boolExpressions) =>
    boolExpressions.Aggregate((Expression)Expression.Constant(false), MakeOrBinary);

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
    var parameter = Expression.Parameter(typeof(T));
    var comparisons = GetPropertyExpressions(parameter, constant);
    var orExpression = MakeOrExpression(comparisons);

    return Expression.Lambda<Func<T, bool>>(orExpression, parameter);
}

WriteLine(MakeSearchExpression<Book>("Shakespeare"));

var books = new []
{
    new Book
    {
        Title = "12 стульев",
        Publisher = new Publisher { Title = "АСТ" },
        Authors = new []
        {
            new Author { GivenName = "Илья", FamilyName = "Ильф" },
            new Author { GivenName = "Евгений", FamilyName = "Петров" }
        }
    },
    new Book
    {
        Title = "Анна Каренина",
        Publisher = new Publisher { Title = "Азбука" },
        Authors = new []
        {
            new Author { GivenName = "Лев", FamilyName = "Толстой" }
        }
    },
    new Book
    {
        Title = "Азбука",
        Publisher = new Publisher { Title = "АСТ" },
        Authors = new []
        {
            new Author { GivenName = "Валентина", FamilyName = "Дмитриева" }
        }
    }
}.AsQueryable();

foreach (var book in books.Where(MakeSearchExpression<Book>("Евгений")))
    WriteLine(book);

foreach (var book in books.Where(MakeSearchExpression<Book>("Азбука")))
    WriteLine(book);

foreach (var book in books.Where(MakeSearchExpression<Book>("АСТ")))
    WriteLine(book);

//

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

var a = 15.0;
Derivative(x => a * x * (x + 3)).ToString()
