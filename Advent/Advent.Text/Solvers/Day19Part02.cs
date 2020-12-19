using System.Linq;
using Advent.Text.Communication;

namespace Advent.Text.Solvers
{
    [Solver("19.2")]
    public class Day19Part02 : Solver
    { 

        public override object Solve()
        {
            var contents = ReadInputFile();

            var data = contents.Split("\n\n");
            var rules = data[0].Trim().Split('\n').ToList();
            rules.Add("8: 42 | 42 8"); // These will override the previous rules
            rules.Add("11: 42 31 | 42 11 31");

            var messages = data[1].Trim().Split('\n');

            var validator = new MonsterMessageValidator(rules);
            var result = messages.Count(validator.Check);

            return result;
        }

    }
}
