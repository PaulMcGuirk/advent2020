using Advent.Text.Luggage;

namespace Advent.Text.Solvers
{
    [Solver("7.2")]
    public class Day07Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var rules = LuggageRules.Parse(contents);

            var result = rules.CountChildrenWithQuantity("shiny gold");

            return result;
        }
    }
}
