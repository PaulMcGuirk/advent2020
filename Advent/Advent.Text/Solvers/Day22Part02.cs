
using System.Linq;
using Advent.Text.Games;

namespace Advent.Text.Solvers
{
    [Solver("22.2")]
    public class Day22Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var pieces = contents.Split("\n\n");
            var playerOneCards = pieces[0].Trim().Split("\n")[1..].Select(int.Parse).ToList();
            var playerTwoCards = pieces[1].Trim().Split("\n")[1..].Select(int.Parse).ToList();

            var combat = new Combat(playerOneCards, playerTwoCards, true);

            var result = combat.Play();

            return result;
        }

    }
}
