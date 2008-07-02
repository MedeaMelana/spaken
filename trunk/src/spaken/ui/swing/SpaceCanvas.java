/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.BasicStroke;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;

import javax.swing.JPanel;

import spaken.model.CommandHistory;
import spaken.model.FixedPoint;
import spaken.model.Point;
import spaken.model.Pos;
import spaken.model.Space;
import spaken.model.rendered.Rendered;

public class SpaceCanvas extends JPanel {

	private Space space;

	private Tool currentTool;

	private CommandHistory history;

	private AffineTransform transform;

	{
		addMouseListener(new MouseAdapter() {

			@Override
			public void mousePressed(MouseEvent e) {
				requestFocus();
			}

		});

		addMouseWheelListener(new MouseWheelListener() {

			public void mouseWheelMoved(MouseWheelEvent e) {
				int rot = e.getWheelRotation();
				scale(-0.5 * rot, mouse2space(new Pos(e.getPoint())));
			}
		});
	}

	public SpaceCanvas() {
		this(new Space(), new CommandHistory());
	}

	public SpaceCanvas(Space space, CommandHistory history) {
		this.space = space;
		this.history = history;

		setOpaque(true);
		setBackground(DrawingConstants.BACKGROUND);
	}

	@Override
	protected void paintComponent(Graphics g1) {
		super.paintComponent(g1);

		Graphics2D g = (Graphics2D) g1;
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);

		AffineTransform trans = getTransform();
		g.transform(trans);

		double pixelSize = getPixelSize();
		g.setStroke(new BasicStroke((float) pixelSize));

		Pos lb = new Pos(0, 0).inverseTransform(trans);
		Pos ro = new Pos(getWidth(), getHeight()).inverseTransform(trans);

		g.setColor(DrawingConstants.AXIS);
		Line2D line = new Line2D.Double();
		line.setLine(lb.x, 0, ro.x, 0);
		g.draw(line);
		line.setLine(0, lb.y, 0, ro.y);
		g.draw(line);

		for (Rendered r : space.render()) {
			r.draw(g, pixelSize);
		}

		if (currentTool != null) {
			currentTool.drawState(g, pixelSize);
		}
	}

	public Space getSpace() {
		return space;
	}

	public void setSpace(Space space) {
		this.space = space;
	}

	public CommandHistory getHistory() {
		return history;
	}

	public void setHistory(CommandHistory history) {
		this.history = history;
	}

	public Tool getTool() {
		return currentTool;
	}

	public void setTool(Tool currentTool) {
		discardTool();
		this.currentTool = currentTool;
		if (currentTool != null) {
			currentTool.install(this);
		}
		refresh();
	}

	private void discardTool() {
		if (currentTool != null) {
			currentTool.uninstall(this);
			currentTool = null;
		}
	}

	public AffineTransform getTransform() {
		if (transform == null) {
			transform = new AffineTransform();
		}
		return transform;
	}

	public Pos space2mouse(Pos pos) {
		return pos.transform(getTransform());
	}

	public Pos mouse2space(Pos pos) {
		return pos.inverseTransform(getTransform());
	}

	public void refresh() {
		repaint();
	}

	public void clear() {
		space.clear();
		repaint();
	}

	public double getPixelSize() {
		return 1 / getTransform().getScaleX();
	}

	public double getPointSelectSize() {
		return DrawingConstants.POINT_SELECT_SIZE / getTransform().getScaleX();
	}

	public Point getPointAt(Pos pos) {
		return space.getPointAt(pos, getPointSelectSize());
	}

	public FixedPoint getFixedPointAt(Pos pos) {
		return space.getFixedPointAt(pos, getPointSelectSize());
	}

	private void scale(double zoom, Pos pos) {
		double scale = Math.pow(2, zoom);

		AffineTransform transform = getTransform();
		transform.translate(pos.x, pos.y);
		transform.scale(scale, scale);
		transform.translate(-pos.x, -pos.y);

		refresh();
	}

}
