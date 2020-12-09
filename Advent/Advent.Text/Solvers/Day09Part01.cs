using System.Linq;
using Advent.Text.Security;

namespace Advent.Text.Solvers
{
    [Solver("9.1")]
    public class Day09Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var numbers = contents.Trim()
                .Split('\n')
                .Select(line => long.Parse(line))
                .ToList();

            var result = XMAS.FindInvalid(numbers, 25);

            return result;
        }
    }
}
