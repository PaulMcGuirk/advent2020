using Advent.Text.Life;

namespace Advent.Text.Solvers
{
    [Solver("24.1")]
    public class Day24Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var tiling = new HexagonalLife();
            tiling.SetInitialState(contents.Trim().Split('\n'));

            var result = tiling.CountActiveTiles();

            return result;
        }
    }
}
