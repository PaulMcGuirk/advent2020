using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("10.2")]
    public class Day10Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var adapters = contents.Trim()
                .Split('\n')
                .Select(line => int.Parse(line))
                .ToList();

            adapters.Sort();
            var builtIn = adapters.Last() + 3;
            adapters.Add(builtIn);

            // counts will hold the number of ways to end with the adapter of
            // the given joltage. There's exactly one way to end with the wall.
            var counts = new Dictionary<int, long> { [0] = 1 };

            foreach (var adapter in adapters)
            {
                // The number of chains that end with an adapter is the total
                // number of chains that the adapter can be added to
                counts[adapter] = Enumerable.Range(1, 3)
                    .Select(diff => counts.GetValueOrDefault(adapter - diff))
                    .Sum();
            }

            return counts[builtIn];
        }
    }
}
