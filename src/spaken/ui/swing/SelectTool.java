package spaken.ui.swing;

import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.geom.AffineTransform;

import spaken.model.Pos;

public class SelectTool extends AbstractTool {
	
	private Pos mouseOld;
	
	protected SelectTool() {
		super("Select");
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		super.mouseDragged(e);
		
		SpaceCanvas canvas = getCanvas();
		
		Pos mouse = getMouse();
		if (mouseOld == null) {
			mouseOld = mouse;
			return;
		}
		
		double dx = mouse.x - mouseOld.x;
		double dy = mouse.y - mouseOld.y;
		
		canvas.getTransform().translate(dx, dy);
		canvas.refresh();
	}
	
	@Override
	public void mousePressed(MouseEvent e) {
		super.mousePressed(e);
		
		mouseOld = getMouse();
	}
	
	@Override
	public void mouseReleased(MouseEvent e) {
		super.mouseReleased(e);
		
		mouseOld = null;
	}

	@Override
	public void mouseWheelMoved(MouseWheelEvent e) {
		super.mouseWheelMoved(e);
		
		int rot = e.getWheelRotation();
		scale(-0.5 * rot);
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		super.mouseClicked(e);
		
		if (e.getClickCount() > 1) {
			if (e.getButton() == MouseEvent.BUTTON3) {
				scale(-1);
			} else {
				scale(1);
			}
		}
	}

	private void scale(double zoom) {
		double scale = Math.pow(2, zoom);
		
		Pos mouse = getMouse();
		
		AffineTransform transform = getCanvas().getTransform();
		transform.translate(mouse.x, mouse.y);
		transform.scale(scale, scale);
		transform.translate(-mouse.x, -mouse.y);
		
		getCanvas().refresh();
	}
}
