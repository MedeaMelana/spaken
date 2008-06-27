package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import spaken.model.ImaginaryPointException;
import spaken.model.Point;
import spaken.model.rendered.RenderedPoint;

public abstract class AbstractTool implements Tool, MouseListener,
		MouseMotionListener {

	protected SpaceCanvas canvas;

	private String name;

	private KeyListener escapeListener = new KeyAdapter() {

		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
				resetState();
				canvas.refresh();
			}
		}

	};

	protected AbstractTool(String name) {
		this.name = name;
	}

	public void install(SpaceCanvas canvas) {
		this.canvas = canvas;
		canvas.addMouseListener(this);
		canvas.addMouseMotionListener(this);
		canvas.addKeyListener(escapeListener);
	}

	public void uninstall(SpaceCanvas canvas) {
		this.canvas = null;
		canvas.removeMouseListener(this);
		canvas.removeMouseMotionListener(this);
		canvas.removeKeyListener(escapeListener);
	}

	public String getName() {
		return name;
	}

	public void mouseDragged(MouseEvent e) {
	}

	public void mouseMoved(MouseEvent e) {
	}

	public void mouseClicked(MouseEvent e) {
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	public void mousePressed(MouseEvent e) {
	}

	public void mouseReleased(MouseEvent e) {
	}

	public void drawState(Graphics2D g, double pixelSize) {
	}

	public void resetState() {
	}

	protected void highlightPoint(Graphics2D g, double pixelSize, Point p) {
		if (p == null) {
			return;
		}
		try {
			new RenderedPoint(p.getPos(), true, DrawingConstants.HIGHLIGHT)
					.draw(g, pixelSize);
		} catch (ImaginaryPointException e) {
			// Blah.
		}
	}

}
