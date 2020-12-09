using System.Linq;
using Advent.Text.Security;

namespace Advent.Text.Solvers
{
    [Solver("9.2")]
    public class Day09Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var numbers = contents.Trim()
                .Split('\n')
                .Select(line => long.Parse(line))
                .ToList();

            var firstInvalid = XMAS.FindInvalid(numbers, 25);

            if (firstInvalid == null)
            {
                return firstInvalid;
            }

            var result = XMAS.FindContiguousSum(numbers, firstInvalid.Value);
            if (result == null)
            {
                return null;
            }
            return result.Min() + result.Max();
        }
    }
}
