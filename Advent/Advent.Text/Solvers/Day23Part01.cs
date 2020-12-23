using System.Linq;
using Advent.Text.Games;

namespace Advent.Text.Solvers
{
    [Solver("23.1")]
    public class Day23Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var nums = contents.Trim().Select(digit => (int)char.GetNumericValue(digit)).ToList();

            var cups = new Cups(nums);
            cups.Play(100);

            var result = string.Concat(cups.GetCups(1).Skip(1));

            return result;
        }
    }
}
