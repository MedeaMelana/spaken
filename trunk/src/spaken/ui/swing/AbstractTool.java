package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.*;

import spaken.model.*;
import spaken.model.commands.AddElementCommand;
import spaken.model.rendered.RenderedPoint;

public abstract class AbstractTool implements Tool, MouseListener,
		MouseWheelListener, MouseMotionListener {

	private SpaceCanvas canvas;

	private Pos mouse;

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
		canvas.addMouseWheelListener(this);
		canvas.addKeyListener(escapeListener);
	}

	public void uninstall(SpaceCanvas canvas) {
		this.canvas = null;
		this.mouse = null;
		canvas.removeMouseListener(this);
		canvas.removeMouseMotionListener(this);
		canvas.removeMouseWheelListener(this);
		canvas.removeKeyListener(escapeListener);
	}

	/**
	 * @return The <tt>SpaceCanvas</tt> this tool is installed on, or
	 *         <tt>null</tt> if this tool is not installed.
	 */
	protected SpaceCanvas getCanvas() {
		return canvas;
	}

	/**
	 * @return The <tt>Space</tt> of the current <tt>SpaceCanvas</tt>, or
	 *         <tt>null</tt> if this tool is not installed.
	 */
	protected Space getSpace() {
		if (canvas == null) {
			return null;
		} else {
			return canvas.getSpace();
		}
	}

	public String getName() {
		return name;
	}

	protected void execute(Command command) {
		canvas.getHistory().execute(command);
	}

	protected void addElement(Element e) {
		execute(new AddElementCommand(canvas, e));
	}

	private void trackMouse(MouseEvent e) {
		mouse = new Pos(e.getX(), e.getY()).inverseTransform(canvas
				.getTransform());
	}

	/**
	 * @return The current location of the mouse, in drawing coordinated (i.e.
	 *         inverse transformed with the <tt>AffineTransform</tt> on the
	 *         canvas), or <tt>null</tt> if the mouse is not inside the
	 *         canvas.
	 */
	protected Pos getMouse() {
		return mouse;
	}

	public void mouseDragged(MouseEvent e) {
		trackMouse(e);
	}

	public void mouseMoved(MouseEvent e) {
		trackMouse(e);
	}

	public void mouseClicked(MouseEvent e) {
		trackMouse(e);
	}

	public void mouseEntered(MouseEvent e) {
		trackMouse(e);
	}

	public void mouseExited(MouseEvent e) {
		mouse = null;
	}

	public void mousePressed(MouseEvent e) {
		trackMouse(e);
	}

	public void mouseReleased(MouseEvent e) {
		trackMouse(e);
	}

	public void mouseWheelMoved(MouseWheelEvent e) {
		trackMouse(e);
	}

	public void drawState(Graphics2D g, double pixelSize) {
	}

	public void resetState() {
		mouse = null;
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
