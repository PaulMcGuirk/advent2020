using System;
using System.Linq;
using Advent.Text.Games;

namespace Advent.Text.Solvers
{
    [Solver("23.2")]
    public class Day23Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var nums = contents.Trim().Select(digit => (int)char.GetNumericValue(digit)).ToList();
            var maxNum = nums.Max();

            var allNums = nums.Union(Enumerable.Range(maxNum + 1, 1000000 - maxNum));

            var cups = new Cups(allNums);

            cups.Play(10000000);

            var starCups = cups.GetCups(1).Take(3).ToList();

            var result = (long)starCups[1] * starCups[2];

            return result;
        }
    }
}
