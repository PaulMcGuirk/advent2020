using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("6.1")]
    public class Day06Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var result = contents.Trim()
                .Split("\n\n")
                .Select(group => group.Replace("\n", "").Distinct().Count())
                .Sum();

            return result;
        }
    }
}
