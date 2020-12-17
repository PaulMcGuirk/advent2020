using Advent.Text.Life;

namespace Advent.Text.Solvers
{
    [Solver("17.2")]
    public class Day17Part02 : Solver
    { 

        public override object Solve()
        {
            var rawInput = ReadInputFile();
            
            var life = new Life4D();
            life.SetStatePlanar(rawInput);

            life.Tick(6);

            return life.CountLiving();
        }

    }
}
