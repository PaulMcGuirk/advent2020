using Advent.Text.VirtualMachines;

namespace Advent.Text.Solvers
{
    [Solver("14.2")]
    public class Day14Part02 : Solver
    {
        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var computer = new BitmaskComputer(BitmaskingTypes.Address);

            computer.RunInstructions(rawInput);

            var result = computer.TotalValuesInMemory();
            
            return result;
        }
    }
}
