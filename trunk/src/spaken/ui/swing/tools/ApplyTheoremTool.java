package spaken.ui.swing.tools;

import java.awt.Graphics2D;
import java.util.List;

import spaken.model.ImaginaryPointException;
import spaken.model.Pos;
import spaken.model.elements.AssumedPoint;
import spaken.model.elements.Point;
import spaken.model.elements.Theorem;

public class ApplyTheoremTool extends AbstractTool {
	private Theorem originalTheorem;
	
	private Theorem currentTheorem;
	private List<AssumedPoint> assumptions;
	private int nextAssumption;
	
	public ApplyTheoremTool(String name, Theorem theorem) {
		super("Apply Theorem '" + name + "'");
		this.originalTheorem = theorem;
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeStarted(Pos origin) {
		if (currentTheorem == null) {
			currentTheorem = originalTheorem.copyElement();
			assumptions = currentTheorem.getAssumptions();
			if (assumptions.isEmpty()) {
				System.err.println("empty");
				// TODO maybe apply theorem as is?
				resetState();
				return;
			} else {
				for (AssumedPoint p : assumptions) {
					p.setActual(getMousePoint());
				}
				
				nextAssumption = 0;
			}
		}
		
		feedPoint(getCanvas().getPointAt(origin));
	}
	
	protected void feedPoint(Point p) {
		assumptions.get(nextAssumption).setActual(p);
		
		nextAssumption++;
		
		if (nextAssumption >= assumptions.size()) {
			addElement(currentTheorem);
			resetState();
		}
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		if (currentTheorem == null) return;
		
		if (! origin.equals(end)) {
			Point p = getCanvas().getPointAt(end);
			feedPoint(p);
		}
		
		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (currentTheorem == null) {
			return;
		}

		for (int i = 0; i < nextAssumption; i++) {
			highlightPoint(g, pixelSize, assumptions.get(i));
		}

		if (isMouseInside()) {
			try {
				// TODO outline
				currentTheorem.render().draw(g, pixelSize);
			} catch (ImaginaryPointException e) {
			}
		}
	}

	@Override
	public void resetState() {
		super.resetState();
		currentTheorem = null;
		// not really necessary, but saves space
		assumptions = null;
	}

}
