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

            var cups = new Cups(nums, 3, 1000000);
            cups.Play(10000000);

            var result = cups.GetCups(1).Take(3).Aggregate(1L, (product, next) => product * next);

            return result;
        }
    }
}
