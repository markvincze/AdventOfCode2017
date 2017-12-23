using System;

namespace day_23_coprocessor
{
    class Program
    {
        static void Main(string[] args)
        {
            var h = 0;
            var b = 105700;

            while (b <= 122700)
            {
                Console.WriteLine("Iteration, b: {0}", b);

                var f = 1;

                // Optimize the upper limit
                for (var d = 2; d < (b / 2) + 1; d++)
                {
                    // Optimize the upper limit
                    for (var e = 2; e < (b / (d - 1)) + 1; e++)
                    {
                        if (e * d == b)
                            f = 0;
                    }
                }

                if (f == 0)
                {
                    h++;
                }

                b += 17;
            } 

            Console.WriteLine("The end value of h is {0}", h);
        }
    }
}