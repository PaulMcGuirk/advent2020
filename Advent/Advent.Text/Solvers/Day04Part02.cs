﻿using System.Linq;
using Advent.Text.TravelDocuments;

namespace Advent.Text.Solvers
{
    [Solver("4.2")]
    public class Day04Part02 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();
            var passports = Passport.ParseBatchString(contents);

            return passports.Where(passport => passport.IsValid()).Count();
        }
    }
}
