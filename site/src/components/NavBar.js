import React from 'react';
import { Link } from 'react-router-dom'
import PatreonButton from './PatreonButton'
import icon from '../assets/logo.svg'
import {title} from '../data/config.json'

export default () =>
    <nav className="navbar" role="navigation" aria-label="main navigation">
        <div>
            <Link className="navbar-title" to="/">
                    <img className="logo" width="24" height="24" src={icon} alt="logo" />
                    <span className="title">{title}</span>
            </Link>
            <span className="subtitle is-size-6"><i> A Chris Penner Project</i></span>
        </div>
        <PatreonButton />
    </nav>
