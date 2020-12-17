using Advent.Text.Life;

namespace Advent.Text.Solvers
{
    [Solver("17.1")]
    public class Day17Part01 : Solver
    { 

        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var life = new Life3D();
            life.SetStatePlanar(rawInput);

            life.Tick(6);

            return life.CountLiving();
        }

    }
}
