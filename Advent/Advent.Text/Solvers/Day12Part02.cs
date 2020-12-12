using Advent.Text.Vehicles;

namespace Advent.Text.Solvers
{
    [Solver("12.1")]
    public class Day12Part01 : Solver
    {
        public override object Solve()
        {
            var instructions = ReadInputFile();

            var ferry = new Ferry(Ferry.Directions.East, Ferry.NavigationModes.Absolute);

            ferry.Navigate(instructions);

            var result = ferry.Position.Magnitude;
            
            return result;
        }
    }
}
