using System;
using System.Collections.Generic;
using System.Linq;
using Advent.Text.Life;

namespace Advent.Text.Solvers
{
    [Solver("11.1")]
    public class Day11Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();
            
            var rules = new Dictionary<char, NextStateRule>
            {
                ['L'] = counts => counts['#'] == 0 ? '#' : 'L',
                ['#'] = counts => counts['#'] >= 4 ? 'L' : '#',
                ['.'] = _ => '.'
            };

            var life = new GenericLife(rules, contents, '.', NeighborCountingRules.Adjacent);

            life.ContinueUntilFixed();

            var result = life.Count('#');

            return result;
        }
    }
}
