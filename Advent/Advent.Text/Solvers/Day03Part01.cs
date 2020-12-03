using Advent.Text.Toboggan;

namespace Advent.Text.Solvers
{
    [Solver("3.1")]
    public class Day03Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();
            var forest = Forest.Parse(contents);

            var slope = new Slope { DeltaX = 3, DeltaY = 1 };

            return forest.CountTileAlongLine(TileType.Tree, slope);
        }
    }
}
