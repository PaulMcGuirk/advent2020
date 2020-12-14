using Advent.Text.VirtualMachines;

namespace Advent.Text.Solvers
{
    [Solver("14.1")]
    public class Day14Part01 : Solver
    {
        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var computer = new BitmaskComputer();

            computer.RunInstructions(rawInput);

            var result = computer.TotalValuesInMemory();
            
            return result;
        }
    }
}
