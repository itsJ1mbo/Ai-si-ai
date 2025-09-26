package es.ucm.fdi.ici.c2526.practica3.grupoYY;

import java.io.File;
import java.util.Iterator;

import jess.Fact;
import jess.JessException;
import jess.Rete;

public class Retetest {

	public static void main(String args[]) {
		final String RULES_PATH = "es"+File.separator+"ucm"+File.separator+"fdi"+File.separator+"ici"+File.separator+"c2526"+File.separator+"practica3"+File.separator+"grupoYY"+File.separator;
		String RULES_FILE = "rulestest.clp";
		String rulesFile = String.format("%s%s", RULES_PATH, RULES_FILE);
		Rete jess = new Rete();
		try {
			jess.batch(rulesFile);
			jess.reset();
			jess.run();
			Iterator<?> it = jess.listFacts();
			while(it.hasNext()){ 
			    Fact dd = (Fact)it.next();
			    System.out.println(dd.toString());
			}
		} catch (JessException e) {
			e.printStackTrace();
		}
	}
}
