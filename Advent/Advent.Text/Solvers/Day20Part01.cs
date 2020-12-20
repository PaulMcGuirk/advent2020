using Advent.Text.ImageProcessing;

namespace Advent.Text.Solvers
{
    [Solver("20.1")]
    public class Day20Part01 : Solver
    { 

        public override object Solve()
        {
            var contents = ReadInputFile();

            var jigsaw = new JigsawCamera(contents);
            var solution = jigsaw.Solve();

            if (solution == null)
            {
                return null;
            }

            var result = solution[0, 0].TileId * solution[0, jigsaw.ImageSizeInTiles - 1].TileId
                * solution[jigsaw.ImageSizeInTiles - 1, 0].TileId * solution[jigsaw.ImageSizeInTiles - 1, jigsaw.ImageSizeInTiles - 1].TileId;

            return result;
        }

    }
}
