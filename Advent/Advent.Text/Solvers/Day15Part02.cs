using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("15.2")]
    public class Day15Part02 : Solver
    { 

        public override object Solve()
        {
            var rawInput = ReadInputFile();

            var nums = rawInput.Split(',').Select(int.Parse).ToList();

            var result = NumberGame(nums, 30000000);
            
            return result;
        }

        private static int NumberGame(List<int> nums, int lastTurn)
        {
            // first just read through the list of numbers
            var mentions = nums.Select((num, index) => (num, turn: index + 1))
                .ToDictionary(pair => pair.num, pair => pair.turn);

            var num = nums.Last();
            var turn = nums.Count + 1;
            int? previousTurn = null; // the previous time that a number was mentioned

            for (; turn <= lastTurn; turn++)
            {
                num = (turn - 1 - previousTurn) ?? 0;

                previousTurn = mentions.ContainsKey(num) ? mentions[num] : null;
                mentions[num] = turn;
            }

            return num;
        }
    }
}
