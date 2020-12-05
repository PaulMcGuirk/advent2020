using System.Linq;
using Advent.Text.TravelDocuments;

namespace Advent.Text.Solvers
{
    [Solver("4.1")]
    public class Day04Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();
            var passports = Passport.ParseBatchString(contents);

            return passports.Where(passport => passport.HasRequiredFields()).Count();
        }
    }
}
