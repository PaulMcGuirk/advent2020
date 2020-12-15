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
            var arraySize = 10000;
            var mentions = new int[arraySize];

            // first just read off the initial numbers
            var turn = 1;
            int num, previousMention;
            for (; turn <= nums.Count; turn++)
            {
                num = nums[turn - 1];
                mentions[num] = turn;
            }

            // then look to the history
            num = 0;
            previousMention = 0;
            for (; turn <= lastTurn; turn++)
            {
                num = previousMention == 0 ? 0 : (turn - 1 - previousMention);
                if (num >= arraySize)
                {
                    arraySize *= 2;
                    Array.Resize(ref mentions, arraySize);
                }
                previousMention = mentions[num];
                mentions[num] = turn;
            }

            return num;
        }
    }
}
