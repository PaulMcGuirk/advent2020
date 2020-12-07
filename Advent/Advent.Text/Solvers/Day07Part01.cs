using Advent.Text.Luggage;

namespace Advent.Text.Solvers
{
    [Solver("7.1")]
    public class Day07Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var rules = LuggageRules.Parse(contents);

            var result = rules.CountAncestors("shiny gold");

            return result;
        }
    }
}
