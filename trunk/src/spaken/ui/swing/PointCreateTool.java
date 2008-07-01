package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;

import spaken.model.*;
import spaken.model.rendered.RenderedPoint;

public class PointCreateTool extends AbstractTool {

	protected PointCreateTool() {
		super("Create Point");
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);

		FixedPoint pt = new FixedPoint(getMouse());
		addElement(pt);
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		super.mouseMoved(e);

		getCanvas().refresh();
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		super.mouseDragged(e);

		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		Pos mouse = getMouse();

		if (mouse != null) {
			new RenderedPoint(mouse, true, DrawingConstants.OUTLINE).draw(g,
					pixelSize);
		}
	}

	@Override
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
	}

}
