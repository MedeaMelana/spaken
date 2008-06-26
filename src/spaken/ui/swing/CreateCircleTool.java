package spaken.ui.swing;

import java.awt.event.MouseEvent;

import spaken.model.Circle;
import spaken.model.Point;
import spaken.model.Pos;

public class CreateCircleTool extends AbstractTool {
	
	private Point center, distFrom;

	protected CreateCircleTool() {
		super("Create Circle");
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		Pos mouse = new Pos(e.getX(), e.getY());
		Point p = canvas.getSpace().getPointAt(mouse, 25);
		if (p == null) {
			return;
		}
		if (center == null) {
			System.out.println("setting center");
			center = p;
		} else if (distFrom == null) {
			System.out.println("setting distFrom");
			distFrom = p;
		} else {
			System.out.println("setting distTo");
			canvas.getSpace().add(new Circle(center, distFrom, p));
			center = null;
			distFrom = null;
			canvas.refresh();
		}
	}

}
