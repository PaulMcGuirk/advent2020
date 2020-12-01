using System;
using System.Collections.Generic;

namespace Advent.Text.Solvers
{
    /// <summary>
    /// Solver for day 1 part 1
    /// </summary>
    [Solver("1.1")]
    public class Day01Part01 : Solver
    {
        public override object Solve()
        {
            var entries = ReadEntries();
            if (entries == null)
            {
                return null;
            }

            var pair = FindPair(entries, 2020);

            if (!pair.HasValue)
            {
                return null;
            }

            var result = pair.Value.Item1 * pair.Value.Item2;
            return result;
        }

        /// <summary>
        /// Find a pair of entries that sum to the given target.
        /// </summary>
        /// <param name="entries">The numbers to draw from</param>
        /// <param name="targetSum">The target sum</param>
        /// <returns>The pair, if found. Null otherwise</returns>
        private (uint, uint)? FindPair(List<uint> entries, uint targetSum)
        {
            for (var i = 0; i < entries.Count; i++)
            {
                var a = entries[i];
                for (var j = 0; j < entries.Count; j++)
                {
                    var b = entries[j];
                    if (a + b == targetSum)
                    {
                        return (a, b);
                    }
                }
            }
            return null;
        }

        /// <summary>
        /// Parse the raw input into a list of numbers
        /// </summary>
        /// <returns>The entries</returns>
        private List<uint> ReadEntries()
        {
            var rawData = ReadInputFile();

            var entries = new List<uint>();

            foreach (var dataString in rawData.Split("\n"))
            {
                if (string.IsNullOrEmpty(dataString))
                {
                    continue;
                }

                if (!uint.TryParse(dataString, out uint entry))
                {
                    return null;
                }

                entries.Add(entry);
            }

            return entries;
        }
    }
}
