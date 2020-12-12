using Advent.Text.Vehicles;

namespace Advent.Text.Solvers
{
    [Solver("12.2")]
    public class Day12Part02 : Solver
    {
        public override object Solve()
        {
            var instructions = ReadInputFile();

            var ferry = new Ferry(Ferry.Directions.East,
                Ferry.NavigationModes.WayPoint,
                new Ferry.Vector { X = 10, Y = -1 });

            ferry.Navigate(instructions);

            var result = ferry.Position.Magnitude;
            
            return result;
        }
    }
}
