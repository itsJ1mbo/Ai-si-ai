package es.ucm.fdi.ici.c2526.practica3.grupoYY;

import java.io.File;
import java.util.EnumMap;
import java.util.HashMap;

import es.ucm.fdi.ici.c2526.practica3.grupoYY.ghosts.GhostsInput;
import es.ucm.fdi.ici.c2526.practica3.grupoYY.ghosts.actions.ChaseAction;
import es.ucm.fdi.ici.c2526.practica3.grupoYY.ghosts.actions.RunAwayAction;
import es.ucm.fdi.ici.rules.RuleEngine;
import es.ucm.fdi.ici.rules.RulesAction;
import es.ucm.fdi.ici.rules.RulesInput;
import es.ucm.fdi.ici.rules.observers.ConsoleRuleEngineObserver;
import pacman.controllers.GhostController;
import pacman.game.Constants.GHOST;
import pacman.game.Constants.MOVE;
import pacman.game.Game;

public class Ghosts  extends GhostController  {
	private static final String RULES_PATH = "es"+File.separator+"ucm"+File.separator+"fdi"+File.separator+"ici"+File.separator+"c2526"+File.separator+"practica3"+File.separator+"grupoYY"+File.separator;
	HashMap<String,RulesAction> map;
	
	EnumMap<GHOST,RuleEngine> ghostRuleEngines;
	
	
	public Ghosts() {
		setName("Ghosts XX");
		setTeam("Team XX");
		
		map = new HashMap<String,RulesAction>();
		//Fill Actions
		RulesAction BLINKYchases = new ChaseAction(GHOST.BLINKY);
		RulesAction INKYchases = new ChaseAction(GHOST.INKY);
		RulesAction PINKYchases = new ChaseAction(GHOST.PINKY);
		RulesAction SUEchases = new ChaseAction(GHOST.SUE);
		RulesAction BLINKYrunsAway = new RunAwayAction(GHOST.BLINKY);
		RulesAction INKYrunsAway = new RunAwayAction(GHOST.INKY);
		RulesAction PINKYrunsAway = new RunAwayAction(GHOST.PINKY);
		RulesAction SUErunsAway = new RunAwayAction(GHOST.SUE);
		
		map.put("BLINKYchases", BLINKYchases);
		map.put("INKYchases", INKYchases);
		map.put("PINKYchases", PINKYchases);
		map.put("SUEchases", SUEchases);	
		map.put("BLINKYrunsAway", BLINKYrunsAway);
		map.put("INKYrunsAway", INKYrunsAway);
		map.put("PINKYrunsAway", PINKYrunsAway);
		map.put("SUErunsAway", SUErunsAway);
		
		ghostRuleEngines = new EnumMap<GHOST,RuleEngine>(GHOST.class);
		for(GHOST ghost: GHOST.values())
		{
			String rulesFile = String.format("%s%srules.clp", RULES_PATH, ghost.name().toLowerCase());
			RuleEngine engine  = new RuleEngine(ghost.name(),rulesFile, map);
			ghostRuleEngines.put(ghost, engine);
			
			//add observer to every Ghost
			//ConsoleRuleEngineObserver observer = new ConsoleRuleEngineObserver(ghost.name(), true);
			//engine.addObserver(observer);
		}
		
		//add observer only to BLINKY
		ConsoleRuleEngineObserver observer = new ConsoleRuleEngineObserver(GHOST.BLINKY.name(), true);
		ghostRuleEngines.get(GHOST.BLINKY).addObserver(observer);
		
	}

	@Override
	public EnumMap<GHOST, MOVE> getMove(Game game, long timeDue) {
		
		//Process input
		RulesInput input = new GhostsInput(game);
		//load facts
		//reset the rule engines
		for(RuleEngine engine: ghostRuleEngines.values()) {
			engine.reset();
			engine.assertFacts(input.getFacts());
		}
		
		EnumMap<GHOST,MOVE> result = new EnumMap<GHOST,MOVE>(GHOST.class);		
		for(GHOST ghost: GHOST.values())
		{
			RuleEngine engine = ghostRuleEngines.get(ghost);
			MOVE move = engine.run(game);
			result.put(ghost, move);
		}
		
		return result;
	}

}
