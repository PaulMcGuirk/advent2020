using Advent.Text.Life;

namespace Advent.Text.Solvers
{
    [Solver("24.2")]
    public class Day24Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var tiling = new HexagonalLife();
            tiling.SetInitialState(contents.Trim().Split('\n'));

            tiling.Run(100);

            var result = tiling.CountActiveTiles();

            return result;
        }
    }
}