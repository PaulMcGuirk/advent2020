using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Security
{
    public static class XMAS
    {
        /// <summary>
        /// Given the list <paramref name="nums"/> and an interger
        /// <paramref name="groupSize"/>, finds the first entry (beyond the
        /// initial <paramref name="groupSize"/> numbers) in
        /// <paramref name="nums"/> that is not the sum of two distinct
        /// numbers amont the <paramref name="groupSize"/> entries
        /// preceding that number
        /// </summary>
        /// <returns>The first number sastisfying this property,
        /// or <c>null</c> if none found.</returns>
        public static long? FindInvalid(IList<long> nums, int groupSize)
        {
            for (var i = groupSize; i < nums.Count; i++)
            {
                var nextNum = nums[i];
                var isValid = false;

                // this check scales quadratically, but it's a quick way to
                // handle the requirement that the number has to be
                // the sum of two different entries
                for (var j = i - groupSize; j < i && !isValid; j++)
                {
                    var diff = nextNum - nums[j];
                    for (var k = j + 1; k < i && !isValid; k++)
                    {
                        isValid = diff == nums[k];
                    }
                }

                if (!isValid)
                {
                    return nextNum;
                }
            }

            return null;
        }

        /// <summary>
        /// Finds a list of adjacent numbers that sum to the given target
        /// </summary>
        /// <param name="nums">The numbers to draw from</param>
        /// <param name="targetSum">The target to try to reach</param>
        /// <returns>The list of numbers that reach the sum, or null if not found</returns>
        public static List<long> FindContiguousSum(List<long> nums, long targetSum)
        {
            for (var i = 0; i < nums.Count; i++)
            {
                var sum = nums[i];
                for (var j = i + 1; j < nums.Count; j++)
                {
                    sum += nums[j];
                    if (sum == targetSum)
                    {
                        return nums.GetRange(i, j - i + 1);
                    }
                }
            }
            return null;
        }
    }
}
