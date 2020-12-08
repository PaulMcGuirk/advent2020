using Advent.Text.VirtualMachines;

namespace Advent.Text.Solvers
{
    [Solver("8.1")]
    public class Day08Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var handheld = new Handheld(contents);

            var result = handheld.IdentifyLoop();

            return result;
        }
    }
}
