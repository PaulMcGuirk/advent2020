using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Solvers
{
    /// <summary>
    /// Solver for day 1 part 2
    /// </summary>
    [Solver("1.2")]
    public class Day01Part02 : Solver
    {
        public override object Solve()
        {
            var entries = ReadEntries();
            if (entries == null)
            {
                return null;
            }

            var tuple = FindTuple(entries, 2020, 3);

            if (tuple == null)
            {
                return null;
            }

            var result = tuple.Aggregate(1, (prod, next) => prod * next);
            return result;
        }

        /// <summary>
        /// Find the first "tuple" of the given size whose sum matches the target
        /// sum
        /// </summary>
        /// <param name="entries">The entries to draw from</param>
        /// <param name="targetSum">The target sum</param>
        /// <param name="tupleSize">The size of the tuple</param>
        /// <returns>A list of size <paramref name="tupleSize"/>, or null if not found</returns>
        private List<int> FindTuple(List<int> entries, int targetSum, int tupleSize)
        {
            // the recursive function to generate all subsets of the given size
            static List<int> FindTupleHelper(List<int> entries, int targetSum, int tupleSize, List<int> working, int index)
            {
                if (working.Count == tupleSize)
                {
                    // if the working set is of the right size, check
                    // if the sum is right
                    if (working.Sum() == targetSum)
                    {
                        return working;
                    }
                    return null;
                }

                if (index >= entries.Count)
                {
                    return null;
                }

                // work on generating the next cases by omitting the current
                // entry and by including the current entry
                return FindTupleHelper(entries, targetSum, tupleSize, working, index + 1) ??
                    FindTupleHelper(entries, targetSum, tupleSize, new List<int>(working) { entries[index] }, index + 1);
            }

            return FindTupleHelper(entries, targetSum, tupleSize, new List<int>(), 0);
        }

        /// <summary>
        /// Parse the raw input into a list of numbers
        /// </summary>
        /// <returns>The entries</returns>
        private List<int> ReadEntries()
        {
            var rawData = ReadInputFile();

            var entries = new List<int>();

            foreach (var dataString in rawData.Split("\n"))
            {
                if (string.IsNullOrEmpty(dataString))
                {
                    continue;
                }

                if (!int.TryParse(dataString, out int entry))
                {
                    return null;
                }

                entries.Add(entry);
            }

            return entries;
        }
    }
}
