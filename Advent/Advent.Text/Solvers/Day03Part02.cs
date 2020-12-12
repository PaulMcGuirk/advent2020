using System.Linq;
using Advent.Text.Vehicles;

namespace Advent.Text.Solvers
{
    [Solver("3.2")]
    public class Day03Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();
            var forest = Toboggan.Parse(contents);

            var slopes = new Slope[] {
                new Slope { DeltaX = 1, DeltaY = 1 },
                new Slope { DeltaX = 3, DeltaY = 1 },
                new Slope { DeltaX = 5, DeltaY = 1 },
                new Slope { DeltaX = 7, DeltaY = 1 },
                new Slope { DeltaX = 1, DeltaY = 2 }
            };

            return slopes.Aggregate(1,
                (product, slope) => product *= forest.CountTileAlongLine(TileType.Tree, slope));
        }
    }
}
