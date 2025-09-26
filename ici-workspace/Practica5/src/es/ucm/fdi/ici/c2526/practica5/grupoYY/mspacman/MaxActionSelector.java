package es.ucm.fdi.ici.c2526.practica5.grupoYY.mspacman;

import java.util.HashMap;
import java.util.Map.Entry;

import es.ucm.fdi.ici.Action;
import es.ucm.fdi.ici.fuzzy.ActionSelector;

/*
 * Executes the action with the highest score
 */
public class MaxActionSelector implements ActionSelector {

	Action[] actions;
	
	public MaxActionSelector(Action[] actions)
	{
		this.actions = actions;
	}
	
	@Override
	public Action selectAction(HashMap<String, Double> fuzzyOutput) {
		double max = Double.NEGATIVE_INFINITY;
		String actionName = null;
		for(Entry<String,Double> entry : fuzzyOutput.entrySet()) {
			double value = entry.getValue();
			if(value > max)
			{
				max = value;
				actionName = entry.getKey(); 
			}
		}
		
		if(actionName==null)
			return null;
		
		for(Action a : actions)
			if(a.getActionId().equals(actionName))
				return a;
		
		return null;
		
	}

}
