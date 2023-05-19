#r "System.Numerics"

using System.Numerics;

IEnumerable<BigInteger> Integers()
{
    var i = BigInteger.One;

    while (true)
        yield return i++;
}

string Format<T>(IEnumerable<T> source, int n = 10)
{
    return string.Join(", ", source.Take(n).Select(x => x?.ToString()));
}

Format(Integers())

IEnumerable<BigInteger> Fibs()
{
    yield return BigInteger.Zero;
    yield return BigInteger.One;

    foreach (var s in Fibs().Zip(Fibs().Skip(1), (a, b) => a + b))
        yield return s;
}

Format(Fibs())

//

IEnumerable<BigInteger> PartialSums(IEnumerable<BigInteger> s)
{
    BigInteger sum = 0;

    foreach (var next in s)
    {
        sum += next;
        yield return sum;
    }
}

Format(PartialSums(Integers()))

IEnumerable<double> PartialSums(IEnumerable<double> s)
{
    double sum = 0;

    foreach (var next in s)
    {
        sum += next;
        yield return sum;
    }
}

IEnumerable<double> PiSummands()
{
    double n = 1;
    var sign = -1;
    yield return 1 / n;

    while (true)
    {
        n += 2;
        yield return sign / n;
        sign = 0 - sign;
    }
}

Format(PiSummands())

IEnumerable<double> PiStream() => PartialSums(PiSummands()).Select(x => 4 * x);

IEnumerable<double> EulerTransform(IEnumerable<double> s)
{
    using var e = s.GetEnumerator();
    if (!e.MoveNext())
        yield break;

    double s1 = e.Current;

    if (!e.MoveNext())
        yield break;

    double s2 = e.Current;

    while (e.MoveNext())
    {
        double s0 = s1;
        s1 = s2;
        s2 = e.Current;

        yield return s2 - Math.Pow(s2 - s1, 2) / (s0 - 2 * s1 + s2);
    }
}

Format(EulerTransform(PiStream()))

IEnumerable<IEnumerable<T>> MakeTableau<T>(
    Func<IEnumerable<T>, IEnumerable<T>> transform,
    Func<IEnumerable<T>> generate)
{
    yield return generate();

    Func<IEnumerable<T>, IEnumerable<T>> next = transform;

    while (true)
    {
        yield return next(generate());

        var previous = next;
        next = s => transform(previous(s));
    }
}

IEnumerable<T> Accelerate<T>(
    Func<IEnumerable<T>, IEnumerable<T>> transform,
    Func<IEnumerable<T>> generate)
{
    var tableau = MakeTableau(transform, generate);

    foreach (var sequence in tableau)
    {
        yield return sequence.First();
    }
}

Format(Accelerate(EulerTransform, PiStream))

IEnumerable<double> LnSummands(double x)
{
    var power = x - 1.0;
    var denominator = 1.0;

    while (true)
    {
        yield return power / denominator;

        power *= -(x - 1.0);
        denominator += 1.0;
    }
}

Format(LnSummands(2.0))

IEnumerable<double> Ln2Stream() => PartialSums(LnSummands(2.0));

Format(Ln2Stream())

Format(Accelerate(EulerTransform, Ln2Stream))
