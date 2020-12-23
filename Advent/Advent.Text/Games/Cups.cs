using System;
using System.Collections.Generic;

namespace Advent.Text.Games
{
    /// <summary>
    /// A game of Crab Cups
    /// </summary>
    public class Cups
    {
        private LinkedList<int> _cups;
        private LinkedListNode<int> _current;
        private Dictionary<int, LinkedListNode<int>> _cupLookup; // maintain this for O(1) lookup
        private readonly int _maxValue;

        /// <summary>
        /// Create a new game of cups with the given collection of entries
        /// </summary>
        /// <param name="nums"></param>
        public Cups(IEnumerable<int> nums)
        {
            _cups = new LinkedList<int>();
            _cupLookup = new Dictionary<int, LinkedListNode<int>>();
            _maxValue = 0;

            foreach (var num in nums)
            {
                _cupLookup[num] = _cups.AddLast(num);
                if (num > _maxValue)
                {
                    _maxValue = num;
                }
            }
            _current = _cups.First;
        }

        /// <summary>
        /// Play a number of rounds
        /// </summary>
        /// <param name="numRounds">The number of rounds to play</param>
        public void Play(int numRounds)
        {
            for (var i = 0; i < numRounds; i++)
            {
                PlayRound();
            }
        }

        /// <summary>
        /// Play a single round of Crab Cups
        /// </summary>
        public void PlayRound()
        {
            var removed = new List<int>();
            for (var i = 0; i < 3; i++)
            {
                var toRemove = _current.GetNextCircular();
                removed.Add(toRemove.Value);
                _cups.Remove(toRemove);
            }

            var destinationValue = _current.Value - 1;
            while (destinationValue <= 0 || removed.Contains(destinationValue))
            {
                destinationValue--;
                if (destinationValue <= 0)
                {
                    destinationValue = _maxValue;
                }
            }

            var destination = _cupLookup[destinationValue];
            for (var i = 0; i < 3; i++)
            {
                var val = removed[2 - i];
                _cupLookup[val] = _cups.AddAfter(destination, val);
            }

            _current = _current.GetNextCircular();
        }

        /// <summary>
        /// Get all of the cups, starting with the input
        /// </summary>
        /// <param name="startingCup">The value of the first cup to get</param>
        /// <returns>All of the cups</returns>
        public IEnumerable<int> GetCups(int startingCup)
        {
            var cup = _cupLookup.GetValueOrDefault(startingCup);
            if (cup == null)
            {
                yield break;
            }
            yield return startingCup;
            

            for (cup = cup.GetNextCircular(); cup.Value != startingCup; cup = cup.GetNextCircular())
            {
                yield return cup.Value;
            }
        }
    }

    public static class Extensions
    {
        /// <summary>
        /// Get the next node in a linked list. If the given node is the last node,
        /// this returns the first node.
        /// </summary>
        /// <param name="list">The node to start with</param>
        /// <returns>The next node</returns>
        public static LinkedListNode<int> GetNextCircular(this LinkedListNode<int> list)
            => list.Next ?? list.List.First;
    }
}
