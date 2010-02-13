package spaken.ui.swing.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

import spaken.model.commands.*;

public class RedoAction extends AbstractAction implements CommandListener {

	private CommandHistory history;

	public RedoAction(CommandHistory history) {
		super("Redo");
		this.history = history;
		history.addListener(this);
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(
				KeyEvent.VK_Z, ActionEvent.META_MASK | ActionEvent.SHIFT_MASK));
		update();
	}

	public void actionPerformed(ActionEvent e) {
		history.redo();
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
		setEnabled(history.canRedo());
	}
}
