package SWHS;

/** \file OutputFormat.java
    \author Thulasi Jegatheesan
    \brief Provides the function for writing outputs
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
        \param E_W change in heat energy in the water: change in thermal energy within the water (J)
    */
    public static void write_output(double T_W, double E_W) throws Exception {
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("T_W = ");
        outputfile.println(T_W);
        outputfile.print("E_W = ");
        outputfile.println(E_W);
        outputfile.close();
    }
}
