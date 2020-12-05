using System.Linq;
using Advent.Text.TravelDocuments;

namespace Advent.Text.Solvers
{
    [Solver("5.2")]
    public class Day05Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();
            var ids = contents.Trim()
                .Split('\n')
                .Select(code => new BoardingPass(code).Id)
                .ToHashSet();

            // skip the front row (first 8 ids) and the back row (last 8 ids)
            return Enumerable.Range(8, 1024 - 8)
                .First(id => !ids.Contains(id) && ids.Contains(id - 1) && ids.Contains(id + 1));
        }
    }
}
