using System;
using System.Collections.Generic;
using System.Linq;
using Advent.Text.Security;

namespace Advent.Text.Solvers
{
    [Solver("10.1")]
    public class Day10Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var adapters = contents.Trim()
                .Split('\n')
                .Select(line => int.Parse(line))
                .ToList();

            adapters.Sort();

            var differences = new List<int> { adapters[0] }; // the wall has effectively 0 jolts
            differences.AddRange(Enumerable.Range(1, adapters.Count - 1)
                .Select(i => adapters[i] - adapters[i - 1]));
            differences.Add(3); // the built-in is always 3 jolts higher than the max

            var counts = Enumerable.Range(0, 4)
                .Select(joltDifference => differences.Count(x => x == joltDifference))
                .ToList();

            return counts[1] * counts[3];
        }
    }
}
