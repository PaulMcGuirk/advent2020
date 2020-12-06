using System.Collections.Generic;
using System.Linq;
using Advent.Text.TravelDocuments;

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
                .Select(group => new HashSet<char>(group.Replace("\n", "")).Count())
                .Sum();

            return result;
        }
    }
}
