import React from 'react';
import { Link } from 'react-router-dom'
import PatreonButton from './PatreonButton'
import lensIcon from '../assets/lens.svg'

export default () =>
    <nav className="navbar" role="navigation" aria-label="main navigation">
        <div>
            <Link className="navbar-title" to="/">
                <span>
                    <img width="24" height="24" src={lensIcon} alt="lens icon" />
                    <span className="title"> Lens by Example </span>
                </span>
            </Link>
            <span className="subtitle is-size-6"><i> A Chris Penner Project</i></span>
        </div>
        <PatreonButton />
    </nav>
