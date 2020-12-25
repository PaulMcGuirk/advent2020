using System;
namespace Advent.Text.Security
{
    public static class Keycard
    {
        public static long GetEncryptionKey(long publicKeyOne, long publicKeyTwo)
        {
            var loopSizeOne = ModLog(7, publicKeyOne, 20201227);
            var result = ModPow(publicKeyTwo, loopSizeOne, 20201227);

            return result;
        }

        /// <summary>
        /// Calculate a discrete logarithm. I.e. solves a^x = b mod p
        /// </summary>
        /// <param name="base_">The base (a)</param>
        /// <param name="pow">The result of the exponentiation (b)</param>
        /// <param name="mod">The divisor (p)</param>
        /// <returns>The logarithm (x)</returns>
        private static long ModLog(long base_, long pow, long mod)
        {
            var num = 1L;
            for (var a = 0L; ; a++)
            {
                if (num == pow)
                {
                    return a; 
                }
                num *= base_;
                num %= mod;
            }
        }

        /// <summary>
        /// Calculate a modular exponentiation, a^b mod p
        /// </summary>
        /// <param name="base_">The base (a)</param>
        /// <param name="exp">The exponentiation (b)</param>
        /// <param name="mod">The divisor (p)</param>
        /// <returns>The result</returns>
        private static long ModPow(long base_, long exp, long mod)
        {
            var result = 1L;
            for (var i = 0; i < exp; i++)
            {
                result *= base_;
                result %= mod;
            }
            return result;
        }
    }

}
