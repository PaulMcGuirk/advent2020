using Advent.Text.VirtualMachines;

namespace Advent.Text.Solvers
{
    [Solver("8.2")]
    public class Day08Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var handheld = new Handheld(contents);

            var result = handheld.RepairLoop();

            return result;
        }
    }
}
