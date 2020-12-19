using System.Linq;
using Advent.Text.Communication;

namespace Advent.Text.Solvers
{
    [Solver("19.1")]
    public class Day19Part01 : Solver
    { 

        public override object Solve()
        {
            var contents = ReadInputFile();

            var data = contents.Split("\n\n");
            var rules = data[0].Trim().Split('\n').ToList();
            var messages = data[1].Trim().Split('\n');

            var validator = new MonsterMessageValidator(rules);
            var result = messages.Count(validator.Check);

            return result;
        }

    }
}
