using System.Linq;
using Advent.Text.TravelDocuments;

namespace Advent.Text.Solvers
{
    [Solver("5.1")]
    public class Day05Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            return contents.Trim()
                .Split('\n')
                .Select(code => new BoardingPass(code).Id)
                .Max();
        }
    }
}
