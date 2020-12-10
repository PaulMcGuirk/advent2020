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
            var builtIn = adapters.Last() + 3; // the built in adapeter is always 3 more jolts than the max in bag
            adapters.Add(builtIn);

            // counts will hold the number of ways to end with the
            // adapter of the given joltage
            // since there's only 1 wall, there's one way to end with the wall
            var counts = new Dictionary<int, long> { [0] = 1 };

            foreach (var adapter in adapters)
            {
                // We can put this adapeter at the end of any valid chain of
                // adapters, so the number of ways to end with this adapter
                // is jut the total number of chains that this adapter
                // can be added to
                counts[adapter] = Enumerable.Range(1, 3)
                    .Select(diff => counts.GetValueOrDefault(adapter - diff))
                    .Sum();
            }

            return counts[builtIn];
        }
    }
}
