using Advent.Text.ImageProcessing;

namespace Advent.Text.Solvers
{
    [Solver("20.2")]
    public class Day20Part02 : Solver
    { 
        public override object Solve()
        {
            var contents = ReadInputFile();

            var seaMonsterTemplate = @"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ";

            var jigsaw = new JigsawCamera(contents);
            var solution = jigsaw.Solve();

            var result = jigsaw.GetRoughness(solution, seaMonsterTemplate);

            return result;
        }

    }
}
