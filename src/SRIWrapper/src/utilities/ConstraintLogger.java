package utilities;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import models.Constraint;
import models.Quantifier;

public class ConstraintLogger {

	static FileWriter output = null;
	static String filename = null;
	static String lastUtterance = "";
	
	public static void initialize(String filename)
	{
		ConstraintLogger.filename = filename;

	}
	
	public static void writeUtterance(String utterance)
	{
		if (utterance.equalsIgnoreCase(lastUtterance))
			return;
		try {
			output = new FileWriter(filename, true);
			output.write("\n>USER: '" + utterance + "'\n");
			output.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		lastUtterance = utterance;
	}
	
	public static void writeSystemUtterance(String utterance)
	{
		if (utterance.equalsIgnoreCase(lastUtterance))
			return;
		try {
			output = new FileWriter(filename, true);
			output.write("\n>SYSTEM: '" + utterance + "'\n");
			output.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		lastUtterance = utterance;
	}
	
	public static void writeExample(String example)
	{
		try {
			output = new FileWriter(filename, true);
			output.write("\nExample: \n" + example + "\n");
			output.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
	}
	
	public static void writeQuantifier(Quantifier q)
	{
		try {
			output = new FileWriter(filename, true);
			output.write("\nQuantifier: \n" + q.toString() + "\n");
			output.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static void writeNewConstraint(Constraint c)
	{
		if (c == null)
			return;
		try {
			output = new FileWriter(filename, true);
			output.write("\n>>>>>>> New constraint added <<<<<<<\n");
			output.write(c.toString());
			output.write("__________________________________\n");
			output.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static void removedConstraint(Constraint c)
	{
		try {
			output = new FileWriter(filename, true);
			output.write(">>>>>>> Constraint removed <<<<<<<\n");
			output.write(c.toString());
			output.write("__________________________________\n");
			output.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static String fullConstraintSpecification(Constraint c)
	{
		StringBuilder sb = new StringBuilder();
		
		return sb.toString();
	}
}
