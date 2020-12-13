using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("13.2")]
    public class Day13Part02 : Solver
    {
        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var lines = rawInput.Trim().Split('\n');
            var pieces = lines[1].Split(',')
                .Select((piece, pos) => (piece, pos))
                .Where(pair => pair.piece != "x")
                .Select(pair => (int.Parse(pair.piece), pair.pos))
                .ToList();

            var result = Contest(pieces);

            return result;
        }

        /// <summary>
        /// Given a list of primes and offsets, finds the smallest number
        /// that for each (prime, offset) pair, is smaller than a multiple
        /// of that prime by that offset.
        /// </summary>
        /// <remarks>The offset can be larger than the prime itself. For example,
        /// we might have (13, 25) meaning that we're looking for a number that's
        /// 25 less than a multiple of 13. It's useful that that number will also
        /// be 12 less than a multiple of 13.</remarks>
        /// <example>[(7, 0), (13, 1) (59, 4)] returns 1715 since that is the
        /// smallest number that is divisible by 7, is 1 less than a multiple
        /// of 13, and is 4 less than a multiple of 59</example>
        /// <param name="primesWithOffsets">The (prime, offset) pairs</param>
        /// <returns>The first number that satisfies this</returns>
        private long Contest(List<(int prime, int offset)> primesWithOffsets)
        {
            var result = 0L;
            var product = 1L;

            // let p1, ..., pk be the primes we're considering
            // we'll write our number as
            // n = a1 + a2 * p1 + a3 * (p2 * p1) + ... + ak * p(k-1) * p(k-2) * ... * p1
            // so that every term is divisible by pi except the first i terms
            // The first offset condition (i.e. involving p1) can then be satisfied by setting a1 alone.
            // Once a1 is set, the second offset condition (i.e. involving p2) can be satisfied
            // by finding a value of a2. In this way, we iteratively get the
            // numbers ak and so get the answer.
            foreach (var (prime, offset) in primesWithOffsets)
            {
                var mod = offset == 0 ? 0 : prime - (offset % prime); // having an offset of a particular value is equivalent to having some remainder
                while (result % prime != mod) // the number of times this loop runs is the number ak
                {
                    result += product;
                }
                product *= prime;
            }

            return result;
        }
    }
}
