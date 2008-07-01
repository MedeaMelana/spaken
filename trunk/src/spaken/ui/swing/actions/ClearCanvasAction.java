package spaken.ui.swing.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import spaken.model.Command;
import spaken.model.CommandListener;
import spaken.model.commands.ClearCanvasCommand;
import spaken.ui.swing.SpaceCanvas;

public class ClearCanvasAction extends AbstractAction implements
		CommandListener {

	private SpaceCanvas canvas;

	public ClearCanvasAction(SpaceCanvas canvas) {
		super("Clear");
		this.canvas = canvas;
		canvas.getHistory().addListener(this);
		update();
	}

	public void actionPerformed(ActionEvent e) {
		canvas.getHistory().execute(new ClearCanvasCommand(canvas));
	}

	public void commandExecuted(Command c) {
		update();
	}

	public void commandRedone(Command c) {
		update();
	}

	public void commandUndone(Command c) {
		update();
	}

	private void update() {
		setEnabled(canvas.getSpace().getElements().iterator().hasNext());
	}

}
